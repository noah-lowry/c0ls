(* Position-based AST search (port of ast-search.ts): given a cursor
   position, find the identifier / type / struct field under it, together
   with the variable environment in scope there. Powers hover, definition
   and completion. *)

open Ast

type found_type = IT_typ of typ | IT_fun of fundecl

type found =
  | FoundIdent of { name : string; ftype : found_type }
  | FoundType of typ
  | FoundField of { struct_decl : structdecl; field : param; expr_str : string }
  | FoundLink of string (* URI of a #use'd file or library header *)

type search_result = { environment : venv option; data : found option }

let nothing env = { environment = env; data = None }

let inside (pos : Loc.pos) (sp : Loc.span option) = Loc.is_inside pos sp

let rec find_type (ty : typ) (env : venv option) (pos : Loc.pos) : search_result =
  match ty.t with
  | Pointer arg | Array arg ->
    if inside pos arg.tloc then find_type arg env pos else nothing env
  | StructT _ | Named _ -> { environment = env; data = Some (FoundType ty) }
  | _ -> nothing env

let rec find_expr (genv : Genv.t) (e : expr) (env : venv option) (pos : Loc.pos) : search_result =
  let recur sub = find_expr genv sub env pos in
  match e.e with
  | Call { callee; args } -> (
    if inside pos callee.iloc then begin
      match Genv.get_function_declaration genv callee.name with
      | Some f ->
        { environment = env; data = Some (FoundIdent { name = callee.name; ftype = IT_fun f }) }
      | None -> nothing env
    end
    else
      match List.find_opt (fun arg -> inside pos (Some arg.eloc)) args with
      | Some arg -> recur arg
      | None -> nothing env)
  | CallPtr { callee; args } -> (
    if inside pos (Some callee.eloc) then recur callee
    else
      match List.find_opt (fun arg -> inside pos (Some arg.eloc)) args with
      | Some arg -> recur arg
      | None -> nothing env)
  | Var id -> (
    match env with
    | None -> nothing env
    | Some venv -> (
      match SMap.find_opt id.name venv with
      | Some entry ->
        { environment = env; data = Some (FoundIdent { name = id.name; ftype = IT_typ entry.ty }) }
      | None -> (
        match Genv.get_function_declaration genv id.name with
        | Some f ->
          { environment = env; data = Some (FoundIdent { name = id.name; ftype = IT_fun f }) }
        | None -> nothing env)))
  | Field f -> (
    if inside pos (Some f.obj.eloc) then recur f.obj
    else
      match f.struct_name with
      | None -> nothing env
      | Some sname -> (
        match Genv.get_struct_definition genv sname with
        | Some ({ s_fields = Some fields; _ } as sd) -> (
          match List.find_opt (fun (fd : param) -> fd.p_id.name = f.field.name) fields with
          | Some field ->
            {
              environment = env;
              data = Some (FoundField { struct_decl = sd; field; expr_str = Print.expr e });
            }
          | None -> nothing env)
        | _ -> nothing env))
  | Logical { left; right; _ } | Binary { left; right; _ } ->
    if inside pos (Some left.eloc) then recur left
    else if inside pos (Some right.eloc) then recur right
    else nothing env
  | Index { obj; index } ->
    if inside pos (Some obj.eloc) then recur obj
    else if inside pos (Some index.eloc) then recur index
    else nothing env
  | Alloc kind -> if inside pos kind.tloc then find_type kind env pos else nothing env
  | HasTag { kind; arg } | Cast { kind; arg } ->
    if inside pos kind.tloc then find_type kind env pos
    else if inside pos (Some arg.eloc) then recur arg
    else nothing env
  | AllocArray { kind; size } ->
    if inside pos kind.tloc then find_type kind env pos
    else if inside pos (Some size.eloc) then recur size
    else nothing env
  | Unary { arg; _ } | Length arg ->
    if inside pos (Some arg.eloc) then recur arg else nothing env
  | Cond { test; cons; alt } ->
    if inside pos (Some test.eloc) then recur test
    else if inside pos (Some cons.eloc) then recur cons
    else if inside pos (Some alt.eloc) then recur alt
    else nothing env
  | IntLit _ | BoolLit _ | StrLit _ | ChrLit _ | Null | Result -> nothing env

let rec find_stmt (genv : Genv.t) (s : stmt) (env : venv option) (pos : Loc.pos) : search_result =
  let expr_here e = find_expr genv e env pos in
  match s.s with
  | Block block -> (
    let env = match block.block_env with Some e -> Some e | None -> env in
    match List.find_opt (fun child -> inside pos (Some child.sloc)) block.body with
    | Some child -> find_stmt genv child env pos
    | None -> nothing env)
  | If { test; cons; alt } ->
    if inside pos (Some test.eloc) then expr_here test
    else if inside pos (Some cons.sloc) then find_stmt genv cons env pos
    else (
      match alt with
      | Some alt when inside pos (Some alt.sloc) -> find_stmt genv alt env pos
      | _ -> nothing env)
  | Return (Some arg) when inside pos (Some arg.eloc) -> expr_here arg
  | Return _ -> nothing env
  | ExprStmt e -> expr_here e
  | VarDecl { kind; id; init } -> (
    if inside pos kind.tloc then find_type kind env pos
    else if inside pos id.iloc then
      (* hovering the variable being declared *)
      { environment = env; data = Some (FoundIdent { name = id.name; ftype = IT_typ kind }) }
    else
      match init with
      | Some init when inside pos (Some init.eloc) -> expr_here init
      | _ -> nothing env)
  | Assign { lhs; rhs; _ } ->
    if inside pos (Some lhs.eloc) then expr_here lhs
    else if inside pos (Some rhs.eloc) then expr_here rhs
    else nothing env
  | For { init; update; test; body; invariants } -> (
    match init with
    | Some init when inside pos (Some init.sloc) -> find_stmt genv init env pos
    | _ -> (
      match update with
      | Some update when inside pos (Some update.sloc) -> find_stmt genv update env pos
      | _ ->
        if inside pos (Some test.eloc) then expr_here test
        else if inside pos (Some body.sloc) then find_stmt genv body env pos
        else (
          match List.find_opt (fun inv -> inside pos (Some inv.eloc)) invariants with
          | Some inv -> expr_here inv
          | None -> nothing env)))
  | While { test; body; invariants } ->
    if inside pos (Some test.eloc) then expr_here test
    else if inside pos (Some body.sloc) then find_stmt genv body env pos
    else (
      match List.find_opt (fun inv -> inside pos (Some inv.eloc)) invariants with
      | Some inv -> expr_here inv
      | None -> nothing env)
  | Update { arg; _ } -> if inside pos (Some arg.eloc) then expr_here arg else nothing env
  | ErrorStmt arg -> if inside pos (Some arg.eloc) then expr_here arg else nothing env
  | Assert { test; _ } -> if inside pos (Some test.eloc) then expr_here test else nothing env
  | Break | Continue -> nothing env

let find_decl (genv : Genv.t) (decl : decl) (pos : Loc.pos) : search_result =
  match decl with
  | FunDecl f -> (
    if inside pos f.returns.tloc then find_type f.returns None pos
    else if inside pos f.fname.iloc then
      { environment = None; data = Some (FoundIdent { name = f.fname.name; ftype = IT_fun f }) }
    else begin
      (* the parameters form the environment for contracts *)
      let env =
        try Some (Progcheck.env_from_params genv f.params) with Err.Type_error _ -> None
      in
      let param_hit =
        List.find_map
          (fun (p : param) ->
            if inside pos p.p_kind.tloc then Some (find_type p.p_kind env pos)
            else if inside pos p.p_id.iloc then
              Some
                {
                  environment = env;
                  data = Some (FoundIdent { name = p.p_id.name; ftype = IT_typ p.p_kind });
                }
            else None)
          f.params
      in
      match param_hit with
      | Some r -> r
      | None -> (
        match
          List.find_opt (fun c -> inside pos (Some c.eloc)) (f.preconds @ f.postconds)
        with
        | Some contract -> find_expr genv contract env pos
        | None -> (
          match f.fbody with
          | Some body when inside pos (Some body.sloc) -> find_stmt genv body env pos
          | _ -> { environment = env; data = None }))
    end)
  | TypeDef { td_def; _ } ->
    if inside pos td_def.p_kind.tloc then find_type td_def.p_kind None pos
    else if inside pos td_def.p_id.iloc then
      {
        environment = None;
        data = Some (FoundType { t = Named td_def.p_id; tloc = td_def.p_id.iloc });
      }
    else nothing None
  | FunTypeDef f -> (
    if inside pos f.returns.tloc then find_type f.returns None pos
    else
      match
        List.find_map
          (fun (p : param) ->
            if inside pos p.p_kind.tloc then Some (find_type p.p_kind None pos) else None)
          f.params
      with
      | Some r -> r
      | None -> nothing None)
  | StructDecl { s_fields = Some fields; _ } -> (
    match
      List.find_map
        (fun (fd : param) ->
          if inside pos fd.p_kind.tloc then Some (find_type fd.p_kind None pos) else None)
        fields
    with
    | Some r -> r
    | None -> nothing None)
  | StructDecl _ -> nothing None
  | UseLib { lib; _ } -> (
    match Project.lib_header_path lib with
    | Some path -> { environment = None; data = Some (FoundLink (Validate.uri_of_path path)) }
    | None -> nothing None)
  | UseFile { path; uf_loc } -> (
    match uf_loc with
    | Some { Loc.source = Some src; _ } ->
      let dir = Filename.dirname (Validate.path_of_uri src) in
      let target = if Filename.is_relative path then Filename.concat dir path else path in
      { environment = None; data = Some (FoundLink (Validate.uri_of_path target)) }
    | _ -> nothing None)

let find_genv (genv : Genv.t) (pos : Loc.pos) (uri : string) : search_result =
  let rec go = function
    | [] -> nothing None
    | decl :: rest -> (
      match decl_loc decl with
      | Some l when l.Loc.source = Some uri && inside pos (Some l) -> find_decl genv decl pos
      | _ -> go rest)
  in
  go (Genv.decls genv)
