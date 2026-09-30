(* Typechecking of top-level declarations (port of typecheck/programs.ts). *)

open Ast
open Exprcheck
open Stmtcheck

module SSet = Flow.SSet

let params_defined (params : param list) : SSet.t =
  List.fold_left (fun s (p : param) -> SSet.add p.p_id.name s) SSet.empty params

let env_from_params (genv : Genv.t) (params : param list) : venv =
  List.fold_left
    (fun env (p : param) ->
      Typerel.check_type_in_declaration ~is_function_arg:true genv p.p_kind;
      if SMap.mem p.p_id.name env then
        Err.type_error ?loc:p.p_id.iloc
          (Printf.sprintf "Parameter %s declared a second time" p.p_id.name)
      else SMap.add p.p_id.name { ty = p.p_kind; def_loc = p.p_id.iloc } env)
    SMap.empty params

(* Typecheck one declaration; returns the function identifiers it uses. *)
let check_declaration (cx : ctx) (decl : decl) (errors : errors) : ident list =
  let genv = cx.genv in
  match decl with
  | UseLib _ | UseFile _ -> []
  | StructDecl { s_fields = None; _ } -> []
  | StructDecl ({ s_fields = Some definitions; _ } as sd) ->
    if Genv.is_library_struct genv sd.s_id.name then
      add_err errors
        {
          Err.msg =
            Printf.sprintf "struct %s is declared in a library and cannot be defined here"
              sd.s_id.name;
          tloc = sd.s_loc;
          severity = Err.Error;
        };
    (match Genv.get_struct_definition genv sd.s_id.name with
    | Some { s_fields = Some _; _ } ->
      add_err errors
        {
          Err.msg =
            Err.with_hints
              (Printf.sprintf "struct %s is defined twice" sd.s_id.name)
              [ "structs can only be defined once" ];
          tloc = sd.s_loc;
          severity = Err.Error;
        }
    | _ -> ());
    let fields = ref SSet.empty in
    List.iter
      (fun (definition : param) ->
        if SSet.mem definition.p_id.name !fields then
          add_err errors
            {
              Err.msg =
                Printf.sprintf "field '%s' used more than once in definition of struct '%s'"
                  definition.p_id.name sd.s_id.name;
              tloc = sd.s_loc;
              severity = Err.Error;
            };
        (match Genv.actual_type genv definition.p_kind with
        | Genv.ANamedFun _ ->
          add_err errors
            {
              Err.msg =
                Err.with_hints "cannot put a function directly in a struct"
                  [ "use a function pointer" ];
              tloc = definition.p_loc;
              severity = Err.Error;
            }
        | Genv.AStruct id -> (
          match Genv.get_struct_definition genv id.name with
          | Some { s_fields = Some _; _ } -> ()
          | _ ->
            add_err errors
              {
                Err.msg =
                  Err.with_hints "struct fields must be defined"
                    [
                      Printf.sprintf
                        "define 'struct %s' or make the field a pointer to a 'struct %s'" id.name
                        id.name;
                    ];
                tloc = definition.p_loc;
                severity = Err.Error;
              })
        | _ -> ());
        fields := SSet.add definition.p_id.name !fields)
      definitions;
    []
  | TypeDef { td_def; td_loc; _ } ->
    if Genv.get_typedef genv td_def.p_id.name <> None then
      add_err errors
        {
          Err.msg = Printf.sprintf "type name '%s' already defined as a type" td_def.p_id.name;
          tloc = td_loc;
          severity = Err.Error;
        };
    if Genv.get_function_declaration genv td_def.p_id.name <> None then
      add_err errors
        {
          Err.msg = Printf.sprintf "type name '%s' already used as a function name" td_def.p_id.name;
          tloc = td_loc;
          severity = Err.Error;
        };
    []
  | FunTypeDef f ->
    if Genv.get_typedef genv f.fname.name <> None then
      add_err errors
        {
          Err.msg = Printf.sprintf "function type name '%s' already defined as a type" f.fname.name;
          tloc = f.floc;
          severity = Err.Error;
        };
    if Genv.get_function_declaration genv f.fname.name <> None then
      add_err errors
        {
          Err.msg =
            Printf.sprintf "function type name '%s' already used as a function name" f.fname.name;
          tloc = f.floc;
          severity = Err.Error;
        };
    catching errors (fun () -> Typerel.check_function_return_type genv f.returns);
    (try
       let env = env_from_params genv f.params in
       let defined = params_defined f.params in
       let functions_used = ref [] in
       List.iter
         (fun anno ->
           check cx env Requires anno bool_t;
           functions_used := !functions_used @ Flow.check_expression_uses defined defined anno)
         f.preconds;
       List.iter
         (fun anno ->
           check cx env (Ensures f.returns) anno bool_t;
           functions_used := !functions_used @ Flow.check_expression_uses defined defined anno)
         f.postconds;
       !functions_used
     with Err.Type_error te ->
       add_err errors te;
       [])
  | FunDecl decl -> (
    catching errors (fun () -> Typerel.check_function_return_type genv decl.returns);
    let functions_used = ref [] in
    (try
       let env = env_from_params genv decl.params in
       let defined = params_defined decl.params in
       List.iter
         (fun anno ->
           catching errors (fun () ->
               check cx env Requires anno bool_t;
               functions_used := !functions_used @ Flow.check_expression_uses defined defined anno))
         decl.preconds;
       List.iter
         (fun anno ->
           catching errors (fun () ->
               check cx env (Ensures decl.returns) anno bool_t;
               functions_used := !functions_used @ Flow.check_expression_uses defined defined anno))
         decl.postconds;
       (* check consistency with previous declarations *)
       catching errors (fun () ->
           match Genv.get_function_declaration genv decl.fname.name with
           | Some previous when previous != decl ->
             if previous.fbody <> None && decl.fbody <> None then
               add_err errors
                 {
                   Err.msg = Printf.sprintf "function %s defined more than once" decl.fname.name;
                   tloc = decl.fname.iloc;
                   severity = Err.Error;
                 };
             if not (Typerel.equal_function_types genv previous decl) then begin
               let oldone = if previous.fbody = None then "declaration" else "definition" in
               let newone = if decl.fbody = None then "declaration" else "definition" in
               add_err errors
                 {
                   Err.msg =
                     Printf.sprintf "function %s for '%s' does not match previous function %s"
                       newone decl.fname.name oldone;
                   tloc = decl.fname.iloc;
                   severity = Err.Error;
                 }
             end
           | _ -> ());
       match decl.fbody with
       | None -> ()
       | Some body ->
         if Genv.is_library_function genv decl.fname.name then
           add_err errors
             {
               Err.msg =
                 Printf.sprintf "function %s is declared in a library header and cannot be defined"
                   decl.fname.name;
               tloc = decl.fname.iloc;
               severity = Err.Error;
             };
         (* temporarily add a body-less copy so recursive calls typecheck *)
         Genv.add_decl genv
           (FunDecl
              {
                decl with
                preconds = [];
                postconds = [];
                fbody = None;
                floc = None;
                fdoc = "";
              });
         (try
            ignore (check_stmt cx env body ~returning:(Some decl.returns) ~in_loop:false errors);
            let constants =
              List.fold_left
                (fun cs anno ->
                  List.fold_left
                    (fun cs (x : ident) -> if SSet.mem x.name defined then SSet.add x.name cs else cs)
                    cs (Flow.expression_free_vars anno))
                SSet.empty decl.postconds
            in
            let analysis = Flow.check_stmt_flow defined constants defined body in
            if decl.returns.t <> Void && not analysis.returns then
              add_err errors
                {
                  Err.msg =
                    Printf.sprintf
                      "function %s has non-void return type but does not return along every path"
                      decl.fname.name;
                  tloc = decl.fname.iloc;
                  severity = Err.Error;
                };
            functions_used := !functions_used @ analysis.functions
          with Err.Type_error te -> add_err errors te);
         Genv.pop_last_decl genv
     with Err.Type_error te -> add_err errors te);
    !functions_used)

type result = { errors : Err.terror list }

let span_key (sp : Loc.span option) =
  match sp with
  | Some l -> (l.Loc.start_p, l.Loc.end_p)
  | None -> ({ Loc.line = 0; col = 0 }, { Loc.line = 0; col = 0 })

let check_program (cx : ctx) (decls : decl list) : result =
  let genv = cx.genv in
  (* each used function identifier, paired with the source file it was used in *)
  let functions_used : (ident * string option) list ref = ref [] in
  let all_errors = ref [] in
  List.iter
    (fun decl ->
      let decl_errors : errors = ref [] in
      let used = check_declaration cx decl decl_errors in
      let source = match decl_loc decl with Some l -> l.Loc.source | None -> None in
      functions_used := !functions_used @ List.map (fun f -> (f, source)) used;
      (* Record which file each error belongs to. *)
      List.iter
        (fun (te : Err.terror) ->
          (match (source, te.tloc) with
          | Some src, Some tl when tl.Loc.source = None -> tl.Loc.source <- Some src
          | _ -> ());
          all_errors := te :: !all_errors)
        (List.rev !decl_errors);
      Genv.add_decl genv decl)
    decls;
  (* Report functions that are used somewhere but never defined. *)
  let reported = Hashtbl.create 16 in
  List.iter
    (fun ((f : ident), (use_src : string option)) ->
      match Genv.get_function_declaration genv f.name with
      | None -> () (* printf/format, or already reported as not-declared *)
      | Some def ->
        (* prototypes living in header files count as library declarations *)
        let in_header =
          match def.floc with
          | Some { Loc.source = Some src; _ } ->
            Util.ends_with ~suffix:".h0" src || Util.ends_with ~suffix:".h1" src
          | _ -> false
        in
        if def.fbody = None && not (Genv.is_library_function genv def.fname.name) && not in_header
        then begin
          let msg = Printf.sprintf "function %s was declared but never defined" f.name in
          (match f.iloc with
          | Some l when l.Loc.source = None -> l.Loc.source <- use_src
          | _ -> ());
          let key = (f.name, span_key f.iloc) in
          if not (Hashtbl.mem reported key) then begin
            Hashtbl.replace reported key ();
            all_errors := { Err.msg; tloc = f.iloc; severity = Err.Error } :: !all_errors
          end;
          (* also flag the declaration itself when it is in the same file *)
          let def_src = match def.floc with Some l -> l.Loc.source | None -> None in
          if use_src = def_src then begin
            (match def.fname.iloc with
            | Some l when l.Loc.source = None -> l.Loc.source <- def_src
            | _ -> ());
            let dkey = (f.name ^ "#decl", span_key def.fname.iloc) in
            if not (Hashtbl.mem reported dkey) then begin
              Hashtbl.replace reported dkey ();
              all_errors := { Err.msg; tloc = def.fname.iloc; severity = Err.Error } :: !all_errors
            end
          end
        end)
    !functions_used;
  { errors = List.rev !all_errors }
