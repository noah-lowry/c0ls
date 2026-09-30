(* The LSP server: wires the validation pipeline and AST search into the
   linol server class. Talks LSP over stdio, which is what Neovim, Vim
   (vim-lsp/coc/ALE), Helix, Emacs (eglot), Kate, etc. expect. *)

open Linol_lwt
open Ast

let ( let* ) = Lwt.bind

(* ---------- conversions ---------- *)

let lsp_pos_of (p : Loc.pos) : Position.t =
  Position.create ~line:(max 0 (p.Loc.line - 1)) ~character:(max 0 (p.Loc.col - 1))

let loc_pos_of (p : Position.t) : Loc.pos =
  { Loc.line = p.Position.line + 1; col = p.Position.character + 1 }

let range_of_span (sp : Loc.span option) : Range.t =
  match sp with
  | Some sp -> Range.create ~start:(lsp_pos_of sp.Loc.start_p) ~end_:(lsp_pos_of sp.Loc.end_p)
  | None ->
    let zero = Position.create ~line:0 ~character:0 in
    Range.create ~start:zero ~end_:zero

let lsp_severity (s : Err.severity) : DiagnosticSeverity.t =
  match s with
  | Err.Error -> DiagnosticSeverity.Error
  | Err.Warning -> DiagnosticSeverity.Warning
  | Err.Information -> DiagnosticSeverity.Information
  | Err.Hint -> DiagnosticSeverity.Hint

let lsp_diagnostic (d : Err.diagnostic) : Diagnostic.t =
  Diagnostic.create
    ~message:(`String d.Err.d_msg)
    ~range:(range_of_span d.Err.d_loc)
    ~severity:(lsp_severity d.Err.d_severity)
    ~source:"c0ls" ()

(* Byte offset of an LSP position within [text]. *)
let offset_of_pos (text : string) (pos : Position.t) : int =
  let n = String.length text in
  let rec find_line i l =
    if l = 0 then i
    else
      match String.index_from_opt text i '\n' with
      | Some j -> find_line (j + 1) (l - 1)
      | None -> n
  in
  min n (find_line 0 pos.Position.line + pos.Position.character)

let is_object_uri (uri : string) : bool =
  let rec contains s sub i =
    i + String.length sub <= String.length s
    && (String.sub s i (String.length sub) = sub || contains s sub (i + 1))
  in
  contains uri ".o0/" 0 || contains uri ".o1/" 0

(* ---------- documentation strings ---------- *)

let mk_code (s : string) : string = "```c0\n" ^ s ^ "\n```"

let markdown (value : string) : MarkupContent.t = MarkupContent.create ~kind:MarkupKind.Markdown ~value

(* Function prototype with its contracts, as shown in hover/completion. *)
let fun_proto (f : fundecl) : string =
  String.concat "\n"
    ((Print.fun_signature f
     :: List.map (fun e -> "//@requires " ^ Print.expr e ^ ";") f.preconds)
    @ List.map (fun e -> "//@ensures " ^ Print.expr e ^ ";") f.postconds)

let fun_doc_markdown (f : fundecl) : string =
  mk_code (fun_proto f) ^ "\n" ^ f.fdoc

(* typedef names known to a genv (for the expression mini-parser) *)
let genv_typeids (genv : Genv.t) : string list =
  List.filter_map
    (function
      | TypeDef { td_def; _ } -> Some td_def.p_id.name
      | FunTypeDef f -> Some f.fname.name
      | _ -> None)
    (Genv.decls genv)

let format_specifier_doc =
  "The number and type of format specifiers must match the arguments provided.\n"
  ^ "Available format specifiers:\n```\n  %s -> string\n  %d -> int\n  %c -> char\n  %% -> literal percent sign\n```"

(* built-in "functions" for signature help *)
let builtin_fundecl (name : string) : (fundecl * string) option =
  let mk returns params doc =
    let params =
      List.map
        (fun (t, n) -> { p_kind = { t; tloc = None }; p_id = { name = n; iloc = None }; p_loc = None })
        params
    in
    Some
      ( {
          returns = { t = returns; tloc = None };
          fname = { name; iloc = None };
          params;
          preconds = [];
          postconds = [];
          fbody = None;
          fdoc = doc;
          floc = None;
          is_local_to = None;
        },
        doc )
  in
  match name with
  | "assert" -> mk Void [ (Bool, "condition") ] "Aborts execution if the condition given is false"
  | "error" -> mk Void [ (String, "message") ] "Prints the given message and aborts execution"
  | "printf" ->
    mk Void [ (String, "msg") ] ("Prints `msg`, replacing each _format specifier_ with an argument.\n" ^ format_specifier_doc)
  | "format" ->
    mk String [ (String, "msg") ]
      ("Returns `msg` but replacing each _format specifier_ with an argument.\n" ^ format_specifier_doc)
  | _ -> None

(* ---------- the server class ---------- *)

class c0_lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super

    method spawn_query_handler f = Linol_lwt.spawn f

    (* last successful typecheck of each open document *)
    val open_files : (string, Genv.t) Hashtbl.t = Hashtbl.create 16

    (* current editor contents of each open document *)
    val overlays : (string, string) Hashtbl.t = Hashtbl.create 16
    val mutable workspace_root : string option = None

    method! config_hover = Some (`Bool true)
    method! config_definition = Some (`Bool true)
    method! config_symbol = Some (`Bool true)

    method! config_completion =
      Some (CompletionOptions.create ~triggerCharacters: [ "."; ">"; "@" ] ~resolveProvider:false ())

    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      {
        c with
        signatureHelpProvider = Some (SignatureHelpOptions.create ~triggerCharacters:[ "("; "," ] ());
        semanticTokensProvider =
          Some
            (`SemanticTokensOptions
               (SemanticTokensOptions.create
                  ~legend:
                    (SemanticTokensLegend.create ~tokenTypes:Semtok.token_types
                       ~tokenModifiers:Semtok.token_modifiers)
                  ~full:(`Bool true) ~range:false ()));
      }

    method! on_req_initialize ~notify_back (i : InitializeParams.t) =
      (match i.InitializeParams.rootUri with
      | Some uri -> workspace_root <- Some (DocumentUri.to_path uri)
      | None -> ());
      (match workspace_root with
      | Some root ->
        (Util.display_path :=
           fun uri_or_path ->
             let path = Validate.path_of_uri uri_or_path in
             if Util.starts_with ~prefix:(root ^ "/") path then
               String.sub path (String.length root + 1) (String.length path - String.length root - 1)
             else Filename.basename path)
      | None -> ());
      super#on_req_initialize ~notify_back i

    (* ---------- diagnostics ---------- *)

    method private validate (notify_back : Jsonrpc2.notify_back) (uri : DocumentUri.t)
        (contents : string) : unit Lwt.t =
      let uri_s = DocumentUri.to_string uri in
      Hashtbl.replace overlays uri_s contents;
      let overlay u = Hashtbl.find_opt overlays u in
      let result =
        try Validate.check_document ~overlay ?workspace_root ~uri:uri_s ~contents ()
        with exn ->
          {
            Validate.diagnostics =
              [
                {
                  Err.d_loc = None;
                  d_msg = "c0ls internal error: " ^ Printexc.to_string exn;
                  d_severity = Err.Error;
                };
              ];
            genv = None;
            notices = [];
          }
      in
      (match result.Validate.genv with
      | Some g -> Hashtbl.replace open_files uri_s g
      | None -> ());
      let diags = List.map lsp_diagnostic result.Validate.diagnostics in
      let* () = notify_back#send_diagnostic diags in
      Lwt_list.iter_s
        (fun msg -> notify_back#send_log_msg ~type_:MessageType.Warning msg)
        result.Validate.notices

    method on_notif_doc_did_open ~notify_back d ~content : unit Lwt.t =
      self#validate notify_back d.TextDocumentItem.uri content

    method on_notif_doc_did_change ~notify_back d _changes ~old_content:_ ~new_content :
        unit Lwt.t =
      self#validate notify_back d.VersionedTextDocumentIdentifier.uri new_content

    method on_notif_doc_did_close ~notify_back:_ d : unit Lwt.t =
      Hashtbl.remove overlays (DocumentUri.to_string d.TextDocumentIdentifier.uri);
      Lwt.return ()

    (* ---------- hover ---------- *)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ _doc :
        Hover.t option Lwt.t =
      let uri_s = DocumentUri.to_string uri in
      Lwt.return
        (match Hashtbl.find_opt open_files uri_s with
        | None -> None
        | Some genv -> (
          let cpos = loc_pos_of pos in
          let sr = Ast_search.find_genv genv cpos uri_s in
          match sr.Ast_search.data with
          | Some (Ast_search.FoundIdent { name; ftype = Ast_search.IT_fun _ }) -> (
            match Genv.get_function_declaration ~filename:uri_s genv name with
            | None -> None
            | Some decl ->
              Some (Hover.create ~contents:(`MarkupContent (markdown (fun_doc_markdown decl))) ()))
          | Some (Ast_search.FoundIdent { name; ftype = Ast_search.IT_typ ty }) ->
            Some
              (Hover.create
                 ~contents:(`MarkupContent (markdown (mk_code (Print.typ ty ^ " " ^ name))))
                 ())
          | Some (Ast_search.FoundType ty) -> (
            match ty.t with
            | Named id -> (
              let real = Genv.full_type_name genv ty in
              let doc =
                match Genv.get_typedef_decl genv id.name with
                | Some (TypeDef { td_doc; _ }) -> td_doc
                | Some (FunTypeDef f) -> f.fdoc
                | _ -> ""
              in
              match doc with
              | "" ->
                Some
                  (Hover.create
                     ~contents:
                       (`MarkupContent (markdown (mk_code ("typedef " ^ real ^ " " ^ id.name))))
                     ())
              | doc ->
                Some
                  (Hover.create
                     ~contents:
                       (`MarkupContent
                          (markdown (mk_code ("typedef " ^ real ^ " " ^ id.name) ^ "\n" ^ doc)))
                     ()))
            | _ ->
              Some
                (Hover.create
                   ~contents:(`MarkupContent (markdown (mk_code (Genv.full_type_name genv ty))))
                   ()))
          | Some (Ast_search.FoundField { field; expr_str; _ }) ->
            Some
              (Hover.create
                 ~contents:
                   (`MarkupContent (markdown (mk_code (Print.typ field.p_kind ^ " " ^ expr_str))))
                 ())
          | _ -> None))

    (* ---------- go to definition ---------- *)

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ _doc : Locations.t option Lwt.t =
      let uri_s = DocumentUri.to_string uri in
      let to_location (sp : Loc.span) : Locations.t option =
        let target = Option.value sp.Loc.source ~default:uri_s in
        if is_object_uri target then None
        else
          Some
            (`Location
               [ Location.create ~uri:(DocumentUri.of_string target) ~range:(range_of_span (Some sp)) ])
      in
      Lwt.return
        (match Hashtbl.find_opt open_files uri_s with
        | None -> None
        | Some genv -> (
          let cpos = loc_pos_of pos in
          let sr = Ast_search.find_genv genv cpos uri_s in
          match sr.Ast_search.data with
          | Some (Ast_search.FoundType ty) -> (
            match ty.t with
            | Named id -> (
              match Genv.get_typedef_decl genv id.name with
              | Some (TypeDef { td_loc = Some l; _ }) -> to_location l
              | Some (FunTypeDef { floc = Some l; _ }) -> to_location l
              | _ -> None)
            | StructT id -> (
              match Genv.get_struct_definition genv id.name with
              | Some { s_loc = Some l; _ } -> to_location l
              | _ -> None)
            | _ -> None)
          | Some (Ast_search.FoundIdent { name; ftype = Ast_search.IT_fun _ }) -> (
            match Genv.get_function_declaration ~filename:uri_s genv name with
            | Some { floc = Some l; _ } -> to_location l
            | _ -> None)
          | Some (Ast_search.FoundIdent { name; ftype = Ast_search.IT_typ _ }) -> (
            match sr.Ast_search.environment with
            | Some env -> (
              match SMap.find_opt name env with
              | Some { def_loc = Some l; _ } -> to_location l
              | _ -> None)
            | None -> None)
          | Some (Ast_search.FoundField { struct_decl; field; _ }) -> (
            match field.p_id.iloc with
            | Some l ->
              let l =
                match (l.Loc.source, struct_decl.s_loc) with
                | None, Some sl -> { l with Loc.source = sl.Loc.source }
                | _ -> l
              in
              to_location l
            | None -> None)
          | Some (Ast_search.FoundLink target) ->
            if is_object_uri target then None
            else
              Some
                (`Location
                   [
                     Location.create ~uri:(DocumentUri.of_string target)
                       ~range:
                         (Range.create
                            ~start:(Position.create ~line:0 ~character:0)
                            ~end_:(Position.create ~line:0 ~character:0));
                   ])
          | _ -> None))

    (* ---------- completion ---------- *)

    method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos ~ctx:_ ~workDoneToken:_
        ~partialResultToken:_ (doc : doc_state) =
      let uri_s = DocumentUri.to_string uri in
      let keywords =
        List.map
          (fun word -> CompletionItem.create ~label:word ~kind:CompletionItemKind.Keyword ())
          Lexer.keywords
      in
      Lwt.return
        (match Hashtbl.find_opt open_files uri_s with
        | None -> Some (`List keywords)
        | Some genv -> Some (`List (self#completions_for genv uri_s doc pos)))

    method private completions_for (genv : Genv.t) (uri_s : string) (doc : doc_state)
        (pos : Position.t) : CompletionItem.t list =
      let contents =
        match Hashtbl.find_opt overlays uri_s with Some c -> c | None -> doc.content
      in
      let cursor = loc_pos_of pos in
      let offset = offset_of_pos contents pos in
      let context = Completions.get_context ~typeids:(genv_typeids genv) contents offset in
      match context with
      | Some Completions.ContractDecl ->
        List.map
          (fun label -> CompletionItem.create ~label ~kind:CompletionItemKind.Keyword ())
          [ "assert"; "loop_invariant"; "requires"; "ensures" ]
      | _ -> (
        let locals = ref [] in
        let functions : (string, CompletionItem.t) Hashtbl.t = Hashtbl.create 16 in
        let fun_order = ref [] in
        let typedefs = ref [] in
        let structs = ref [] in
        let field_result = ref None in
        let location_of (d : decl) : string option =
          match decl_loc d with
          | Some { Loc.source = Some src; _ } ->
            let display = !Util.display_path src in
            if Util.ends_with ~suffix:".h0" src then
              Some ("#use <" ^ Filename.remove_extension (Filename.basename src) ^ ">")
            else Some display
          | _ -> None
        in
        (try
           List.iter
             (fun d ->
               let in_current_file =
                 match decl_loc d with
                 | Some { Loc.source = Some src; _ } -> src = uri_s
                 | _ -> false
               in
               (* stop at declarations after the cursor in the current file *)
               (match decl_loc d with
               | Some l when in_current_file && Loc.compare_pos cursor l.Loc.start_p = Loc.Less ->
                 raise Exit
               | _ -> ());
               let detail = location_of d in
               match d with
               | TypeDef { td_def; td_doc; _ } ->
                 typedefs :=
                   CompletionItem.create ~label:td_def.p_id.name
                     ~kind:CompletionItemKind.Interface
                     ~documentation:
                       (`MarkupContent
                          (markdown
                             (mk_code
                                ("typedef " ^ Print.typ td_def.p_kind ^ " " ^ td_def.p_id.name)
                             ^ "\n" ^ td_doc)))
                     ?detail ()
                   :: !typedefs
               | FunTypeDef f ->
                 typedefs :=
                   CompletionItem.create ~label:f.fname.name ~kind:CompletionItemKind.Interface
                     ~documentation:
                       (`MarkupContent (markdown (mk_code ("typedef " ^ Print.fun_signature f))))
                     ?detail ()
                   :: !typedefs
               | StructDecl sd ->
                 structs :=
                   CompletionItem.create
                     ~label:("struct " ^ sd.s_id.name)
                     ~kind:CompletionItemKind.Struct
                     ~documentation:
                       (`MarkupContent
                          (markdown (mk_code ("struct " ^ sd.s_id.name) ^ "\n" ^ sd.s_doc)))
                     ?detail ()
                   :: !structs
               | FunDecl f ->
                 (* prefer contracts from a definition in the current file, or a
                    prototype from another file *)
                 let show = in_current_file || f.fbody = None in
                 if
                   show
                   && ((not (Hashtbl.mem functions f.fname.name))
                      || (in_current_file && f.fbody <> None)
                      || ((not in_current_file) && f.fbody = None))
                 then begin
                   if not (Hashtbl.mem functions f.fname.name) then
                     fun_order := f.fname.name :: !fun_order;
                   Hashtbl.replace functions f.fname.name
                     (CompletionItem.create ~label:f.fname.name ~kind:CompletionItemKind.Function
                        ~documentation:(`MarkupContent (markdown (fun_doc_markdown f)))
                        ?detail ())
                 end;
                 (* look inside the function under the cursor for locals *)
                 if in_current_file && Loc.is_inside cursor f.floc then begin
                   let sr = Ast_search.find_decl genv d cursor in
                   match sr.Ast_search.environment with
                   | None -> ()
                   | Some env -> (
                     (match context with
                     | Some (Completions.StructAccess { expr; dereferenced }) -> (
                       try
                         let cx = { Exprcheck.genv; source_file = Some uri_s } in
                         let synthed = Exprcheck.synth cx env Exprcheck.Ordinary expr in
                         let actual =
                           match Typerel.actual_synthed genv synthed with
                           | Typerel.Act (Genv.APointer arg) when dereferenced ->
                             Some (Genv.actual_type genv arg)
                           | Typerel.Act a when not dereferenced -> Some a
                           | Typerel.Act a -> Some a
                           | _ -> None
                         in
                         match actual with
                         | Some (Genv.AStruct sid) -> (
                           match Genv.get_struct_definition genv sid.name with
                           | Some { s_fields = Some fields; s_id; _ } ->
                             field_result :=
                               Some
                                 (List.map
                                    (fun (fd : param) ->
                                      CompletionItem.create ~label:fd.p_id.name
                                        ~kind:CompletionItemKind.Field
                                        ~documentation:
                                          (`MarkupContent
                                             (markdown
                                                (mk_code
                                                   (Printf.sprintf "struct %s {\n  ...\n  %s %s;\n};"
                                                      s_id.name (Print.typ fd.p_kind) fd.p_id.name))))
                                        ())
                                    fields);
                             raise Exit
                           | _ -> ())
                         | _ -> ()
                       with
                       | Exit -> raise Exit
                       | _ -> ())
                     | _ -> ());
                     SMap.iter
                       (fun name (entry : env_entry) ->
                         locals :=
                           CompletionItem.create ~label:name ~kind:CompletionItemKind.Variable
                             ~documentation:
                               (`MarkupContent
                                  (markdown (mk_code (Print.typ entry.ty ^ " " ^ name))))
                             ()
                           :: !locals)
                       env)
                 end
               | UseLib _ | UseFile _ -> ())
             (Genv.decls genv)
         with Exit -> ());
        match !field_result with
        | Some fields -> fields
        | None ->
          let builtins =
            [
              CompletionItem.create ~label:"assert" ~kind:CompletionItemKind.Function
                ~documentation:(`MarkupContent (markdown (mk_code "void assert(bool condition)")))
                ~detail:"<C0 built-in assert>" ();
              CompletionItem.create ~label:"error" ~kind:CompletionItemKind.Function
                ~documentation:(`MarkupContent (markdown (mk_code "void error(string message)")))
                ~detail:"<C0 built-in error>" ();
              CompletionItem.create ~label:"alloc" ~kind:CompletionItemKind.Function
                ~documentation:(`MarkupContent (markdown (mk_code "t* alloc(t)")))
                ~detail:"<C0 built-in alloc>" ();
              CompletionItem.create ~label:"alloc_array" ~kind:CompletionItemKind.Function
                ~documentation:(`MarkupContent (markdown (mk_code "t[] alloc_array(t, int count)")))
                ~detail:"<C0 built-in alloc_array>" ();
            ]
            @ (if Genv.SSet.mem "conio" genv.Genv.libs_loaded then
                 [
                   CompletionItem.create ~label:"printf" ~kind:CompletionItemKind.Function
                     ~documentation:
                       (`MarkupContent
                          (markdown
                             (mk_code "void printf(string msg, ...args)"
                             ^ "\nPrints the message and argument")))
                     ~detail:"<conio>" ();
                 ]
               else [])
            @
            if Genv.SSet.mem "string" genv.Genv.libs_loaded then
              [
                CompletionItem.create ~label:"format" ~kind:CompletionItemKind.Function
                  ~documentation:
                    (`MarkupContent
                       (markdown
                          (mk_code "string format(string msg, ...args)"
                          ^ "\nReturns the message and arguments formatted as a string.")))
                  ~detail:"<string>" ();
              ]
            else []
          in
          let ordered_functions =
            List.rev_map (fun name -> Hashtbl.find functions name) !fun_order
          in
          List.rev !locals @ ordered_functions @ List.rev !typedefs @ List.rev !structs @ builtins)

    (* ---------- document symbols ---------- *)

    method! on_req_symbol ~notify_back:_ ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ () =
      let uri_s = DocumentUri.to_string uri in
      Lwt.return
        (match Hashtbl.find_opt open_files uri_s with
        | None -> None
        | Some genv ->
          let symbols =
            List.filter_map
              (fun d ->
                let in_file =
                  match decl_loc d with
                  | Some { Loc.source = Some src; _ } -> src = uri_s
                  | _ -> false
                in
                if not in_file then None
                else
                  let mk name kind ?detail range sel =
                    Some
                      (DocumentSymbol.create ~name ~kind ?detail ~range:(range_of_span range)
                         ~selectionRange:(range_of_span (Some sel)) ())
                  in
                  match d with
                  | FunDecl f -> (
                    match (f.floc, f.fname.iloc) with
                    | Some l, Some il ->
                      mk f.fname.name
                        (if f.fbody = None then SymbolKind.Interface else SymbolKind.Function)
                        ~detail:(Print.fun_signature f) (Some l) il
                    | _ -> None)
                  | StructDecl sd -> (
                    match (sd.s_loc, sd.s_id.iloc) with
                    | Some l, Some il -> mk ("struct " ^ sd.s_id.name) SymbolKind.Struct (Some l) il
                    | _ -> None)
                  | TypeDef { td_def; td_loc; _ } -> (
                    match (td_loc, td_def.p_id.iloc) with
                    | Some l, Some il ->
                      mk td_def.p_id.name SymbolKind.Class
                        ~detail:("typedef " ^ Print.typ td_def.p_kind) (Some l) il
                    | _ -> None)
                  | FunTypeDef f -> (
                    match (f.floc, f.fname.iloc) with
                    | Some l, Some il ->
                      mk f.fname.name SymbolKind.Class ~detail:"function typedef" (Some l) il
                    | _ -> None)
                  | UseLib _ | UseFile _ -> None)
              (Genv.decls genv)
          in
          Some (`DocumentSymbol symbols))

    (* ---------- signature help ---------- *)

    method private signature_help (params : SignatureHelpParams.t) : SignatureHelp.t =
      let empty = SignatureHelp.create ~signatures:[] () in
      let uri_s = DocumentUri.to_string params.SignatureHelpParams.textDocument.uri in
      match Hashtbl.find_opt open_files uri_s with
      | None -> empty
      | Some genv -> (
        match Hashtbl.find_opt overlays uri_s with
        | None -> empty
        | Some contents -> (
          let offset = offset_of_pos contents params.SignatureHelpParams.position - 1 in
          match Completions.get_context ~typeids:(genv_typeids genv) contents offset with
          | Some (Completions.FunctionCall { name; argument_number }) -> (
            let decl =
              match Genv.get_function_declaration ~filename:uri_s genv name with
              | Some f -> Some (f, f.fdoc)
              | None -> builtin_fundecl name
            in
            match decl with
            | None -> empty
            | Some (f, doc) ->
              let prefix = Print.typ f.returns ^ " " ^ f.fname.name ^ "(" in
              let buf = Buffer.create 64 in
              Buffer.add_string buf prefix;
              let parameters =
                List.mapi
                  (fun i (p : param) ->
                    if i > 0 then Buffer.add_string buf ", ";
                    let start_ofs = Buffer.length buf in
                    let text = Print.typ p.p_kind ^ " " ^ p.p_id.name in
                    Buffer.add_string buf text;
                    ParameterInformation.create
                      ~label:(`Offset (start_ofs, start_ofs + String.length text))
                      ())
                  f.params
              in
              Buffer.add_string buf ")";
              let signature =
                SignatureInformation.create ~label:(Buffer.contents buf)
                  ~documentation:(`MarkupContent (markdown doc))
                  ~parameters ()
              in
              SignatureHelp.create ~signatures:[ signature ] ~activeSignature:0
                ~activeParameter:(Some argument_number) ())
          | _ -> empty))

    (* ---------- semantic tokens ---------- *)

    method private semantic_tokens (params : SemanticTokensParams.t) : SemanticTokens.t option =
      let uri_s = DocumentUri.to_string params.SemanticTokensParams.textDocument.uri in
      match Hashtbl.find_opt overlays uri_s with
      | None -> None
      | Some text ->
        let genv = Hashtbl.find_opt open_files uri_s in
        let data = Semtok.compute genv ~uri:uri_s ~text in
        Some (SemanticTokens.create ~data ())

    method! on_request_unhandled : type r.
        notify_back:Jsonrpc2.notify_back ->
        id:Jsonrpc2.Req_id.t ->
        r Linol_lsp.Lsp.Client_request.t ->
        r Lwt.t =
      fun ~notify_back ~id r ->
        match r with
        | Linol_lsp.Lsp.Client_request.SignatureHelp params ->
          Lwt.return (self#signature_help params)
        | Linol_lsp.Lsp.Client_request.SemanticTokensFull params ->
          Lwt.return (self#semantic_tokens params)
        | _ -> super#on_request_unhandled ~notify_back ~id r
  end

let run () : unit =
  let s = new c0_lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio ~env:() (s :> Linol_lwt.Jsonrpc2.server) in
  let task = Linol_lwt.Jsonrpc2.run ~shutdown:(fun () -> s#get_status = `ReceivedExit) server in
  Linol_lwt.run task
