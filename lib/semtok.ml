(* Semantic tokens (textDocument/semanticTokens/full).

   Two passes:
   1. a lexical pass over the current text colors keywords, literals,
      comments, operators, pragmas and contract annotations;
   2. identifiers get their color from the last good AST: function names,
      parameters, local variables, typedef names, struct names and struct
      fields are told apart using what the parser/typechecker knew.

   Because the server supplies the complete token stream, editors get full
   highlighting for C0 without needing any syntax file. *)

open Ast

(* The legend. Indices below must match positions in these lists. *)
let token_types =
  [ "type"; "struct"; "parameter"; "variable"; "property"; "function";
    "macro"; "keyword"; "comment"; "string"; "number"; "operator" ]

let token_modifiers = [ "declaration"; "defaultLibrary" ]

let t_type = 0
let t_struct = 1
let t_parameter = 2
let t_variable = 3
let t_property = 4
let t_function = 5
let t_macro = 6
let t_keyword = 7
let t_comment = 8
let t_string = 9
let t_number = 10
let t_operator = 11

let m_declaration = 1 (* 1 lsl 0 *)
let m_default_library = 2 (* 1 lsl 1 *)

(* ------------------------------------------------------------------ *)
(* Pass 2 data: identifier classification from the AST                 *)
(* ------------------------------------------------------------------ *)

module PosMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

type classifier = { mutable map : (int * int) PosMap.t } (* pos -> (type, mods) *)

let add_ident (c : classifier) (id : ident) (ty : int) (mods : int) : unit =
  match id.iloc with
  | Some sp -> c.map <- PosMap.add (sp.Loc.start_p.line, sp.Loc.start_p.col) (ty, mods) c.map
  | None -> ()

let rec classify_type (c : classifier) (ty : typ) : unit =
  match ty.t with
  | Int | Bool | String | Char | Void -> ()
  | Pointer arg | Array arg -> classify_type c arg
  | StructT id -> add_ident c id t_struct 0
  | Named id -> add_ident c id t_type 0

(* [params] is the set of parameter names of the enclosing function, used to
   tell parameters apart from locals. *)
let rec classify_expr (c : classifier) (genv : Genv.t option) (params : (string, unit) Hashtbl.t)
    (e : expr) : unit =
  let recur = classify_expr c genv params in
  match e.e with
  | Var id ->
    if Hashtbl.mem params id.name then add_ident c id t_parameter 0
    else add_ident c id t_variable 0
  | IntLit _ | StrLit _ | ChrLit _ | BoolLit _ | Null | Result -> ()
  | Index { obj; index } ->
    recur obj;
    recur index
  | Field f ->
    recur f.obj;
    add_ident c f.field t_property 0
  | Call { callee; args } ->
    let mods =
      match genv with
      | Some g when Genv.is_library_function g callee.name -> m_default_library
      | _ -> 0
    in
    add_ident c callee t_function mods;
    List.iter recur args
  | CallPtr { callee; args } ->
    recur callee;
    List.iter recur args
  | Cast { kind; arg } | HasTag { kind; arg } ->
    classify_type c kind;
    recur arg
  | Unary { op = UAddrOf; arg } -> (
    (* &f takes the address of a function *)
    match arg.e with
    | Var id ->
      let mods =
        match genv with
        | Some g when Genv.is_library_function g id.name -> m_default_library
        | _ -> 0
      in
      add_ident c id t_function mods
    | _ -> recur arg)
  | Unary { arg; _ } | Length arg -> recur arg
  | Binary { left; right; _ } | Logical { left; right; _ } ->
    recur left;
    recur right
  | Cond { test; cons; alt } ->
    recur test;
    recur cons;
    recur alt
  | Alloc kind -> classify_type c kind
  | AllocArray { kind; size } ->
    classify_type c kind;
    recur size

let rec classify_stmt (c : classifier) (genv : Genv.t option) (params : (string, unit) Hashtbl.t)
    (s : stmt) : unit =
  let expr = classify_expr c genv params in
  match s.s with
  | Assign { lhs; rhs; _ } ->
    expr lhs;
    expr rhs
  | Update { arg; _ } -> expr arg
  | ExprStmt e -> expr e
  | VarDecl { kind; id; init } ->
    classify_type c kind;
    add_ident c id t_variable m_declaration;
    Option.iter expr init
  | If { test; cons; alt } ->
    expr test;
    classify_stmt c genv params cons;
    Option.iter (classify_stmt c genv params) alt
  | While { invariants; test; body } ->
    List.iter expr invariants;
    expr test;
    classify_stmt c genv params body
  | For { invariants; init; test; update; body } ->
    List.iter expr invariants;
    Option.iter (classify_stmt c genv params) init;
    expr test;
    Option.iter (classify_stmt c genv params) update;
    classify_stmt c genv params body
  | Return arg -> Option.iter expr arg
  | Block { body; _ } -> List.iter (classify_stmt c genv params) body
  | Assert { test; _ } -> expr test
  | ErrorStmt e -> expr e
  | Break | Continue -> ()

let classify_fundecl (c : classifier) (genv : Genv.t option) (f : fundecl) ~(name_type : int) :
    unit =
  classify_type c f.returns;
  add_ident c f.fname name_type m_declaration;
  let params = Hashtbl.create 8 in
  List.iter
    (fun (p : param) ->
      Hashtbl.replace params p.p_id.name ();
      classify_type c p.p_kind;
      add_ident c p.p_id t_parameter m_declaration)
    f.params;
  List.iter (classify_expr c genv params) f.preconds;
  List.iter (classify_expr c genv params) f.postconds;
  Option.iter (classify_stmt c genv params) f.fbody

let classify_decls (genv : Genv.t option) (uri : string) : (int * int) PosMap.t =
  let c = { map = PosMap.empty } in
  (match genv with
  | None -> ()
  | Some g ->
    List.iter
      (fun d ->
        let in_file =
          match decl_loc d with Some { Loc.source = Some src; _ } -> src = uri | _ -> false
        in
        if in_file then
          match d with
          | FunDecl f -> classify_fundecl c genv f ~name_type:t_function
          | FunTypeDef f -> classify_fundecl c genv f ~name_type:t_type
          | TypeDef { td_def; _ } ->
            classify_type c td_def.p_kind;
            add_ident c td_def.p_id t_type m_declaration
          | StructDecl sd ->
            add_ident c sd.s_id t_struct m_declaration;
            Option.iter
              (List.iter (fun (p : param) ->
                   classify_type c p.p_kind;
                   add_ident c p.p_id t_property m_declaration))
              sd.s_fields
          | UseLib _ | UseFile _ -> ())
      (Genv.decls g));
  c.map

(* ------------------------------------------------------------------ *)
(* Pass 1: lexical tokens over the current text                        *)
(* ------------------------------------------------------------------ *)

let contract_keywords = [ "requires"; "ensures"; "loop_invariant"; "assert" ]
let backslash_keywords = [ "result"; "length"; "hastag" ]

let punctuation = [ "("; ")"; "["; "]"; "{"; "}"; ","; ";"; "."; "->" ]

(* raw tokens: (line, col, length, type, modifiers), all 1-based *)
let lex_tokens (semantic : (int * int) PosMap.t) (lang : Lang.t) (text : string) :
    (int * int * int * int * int) list =
  let annos = match lang with Lang.C0 | Lang.C1 -> true | _ -> false in
  let lx = Lexer.create ~annos text in
  let out = ref [] in
  let in_anno = ref false in
  let after_backslash = ref false in
  let emit (sp : Loc.span) ty mods =
    (* multi-line tokens (block comments) are emitted per line *)
    if sp.Loc.start_p.line = sp.Loc.end_p.line then begin
      let len = sp.Loc.end_p.col - sp.Loc.start_p.col in
      if len > 0 then out := (sp.Loc.start_p.line, sp.Loc.start_p.col, len, ty, mods) :: !out
    end
    else begin
      let lines = Array.of_list (String.split_on_char '\n' text) in
      for line = sp.Loc.start_p.line to sp.Loc.end_p.line do
        let line_len =
          if line - 1 < Array.length lines then String.length lines.(line - 1) else 0
        in
        let start_col = if line = sp.Loc.start_p.line then sp.Loc.start_p.col else 1 in
        let end_col = if line = sp.Loc.end_p.line then sp.Loc.end_p.col else line_len + 1 in
        let len = end_col - start_col in
        if len > 0 then out := (line, start_col, len, ty, mods) :: !out
      done
    end
  in
  let stop = ref false in
  while not !stop do
    let tok = Lexer.next lx in
    let sp = tok.Lexer.span in
    let was_backslash = !after_backslash in
    after_backslash := false;
    (match tok.Lexer.tok with
    | Lexer.TEOF -> stop := true
    | Lexer.TKw _ -> emit sp t_keyword 0
    | Lexer.TNum _ -> emit sp t_number 0
    | Lexer.TStr _ | Lexer.TChr _ -> emit sp t_string 0
    | Lexer.TComment _ -> emit sp t_comment 0
    | Lexer.TPragma _ -> emit sp t_macro 0
    | Lexer.TAnnoStart | Lexer.TLAnnoStart ->
      in_anno := true;
      emit sp t_macro 0
    | Lexer.TAnnoEnd ->
      in_anno := false;
      emit sp t_macro 0
    | Lexer.TLAnnoEnd -> in_anno := false
    | Lexer.TBackslash ->
      after_backslash := true;
      emit sp t_macro 0
    | Lexer.TIdent name ->
      if was_backslash && List.mem name backslash_keywords then emit sp t_macro 0
      else if !in_anno && List.mem name contract_keywords then emit sp t_keyword 0
      else begin
        match PosMap.find_opt (sp.Loc.start_p.line, sp.Loc.start_p.col) semantic with
        | Some (ty, mods) -> emit sp ty mods
        | None -> emit sp t_variable 0
      end
    | Lexer.TSym s -> if not (List.mem s punctuation) then emit sp t_operator 0)
  done;
  List.rev !out

(* ------------------------------------------------------------------ *)
(* Delta encoding                                                      *)
(* ------------------------------------------------------------------ *)

let encode (tokens : (int * int * int * int * int) list) : int array =
  let sorted = List.sort compare tokens in
  let data = Array.make (5 * List.length sorted) 0 in
  let prev_line = ref 0 and prev_col = ref 0 in
  List.iteri
    (fun i (line, col, len, ty, mods) ->
      let line0 = line - 1 and col0 = col - 1 in
      let dl = line0 - !prev_line in
      let dc = if dl = 0 then col0 - !prev_col else col0 in
      data.((5 * i) + 0) <- dl;
      data.((5 * i) + 1) <- dc;
      data.((5 * i) + 2) <- len;
      data.((5 * i) + 3) <- ty;
      data.((5 * i) + 4) <- mods;
      prev_line := line0;
      prev_col := col0)
    sorted;
  data

(* The full computation: [genv] is the last good typecheck (may be stale or
   absent — identifiers then fall back to plain variables). *)
let compute (genv : Genv.t option) ~(uri : string) ~(text : string) : int array =
  let semantic = classify_decls genv uri in
  let lang = Option.value (Lang.of_filename uri) ~default:Lang.C1 in
  encode (lex_tokens semantic lang text)
