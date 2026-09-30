(* Flow analysis (port of typecheck/flow.ts):
   - every local must be defined on all control paths before use;
   - a non-void function must return along every path;
   - collects the free functions of each declaration so the program checker
     can report functions that are declared and used but never defined. *)

open Ast

module SSet = Set.Make (String)

(* Free variables of an expression, in order of occurrence. Names bound by
   the enclosing function that are not locals are function references. *)
let rec expression_free_vars (exp : expr) : ident list =
  match exp.e with
  | Var id -> [ id ]
  | IntLit _ | StrLit _ | ChrLit _ | BoolLit _ | Null | Result -> []
  | Alloc _ -> []
  | Index { obj; index } -> expression_free_vars index @ expression_free_vars obj
  | Field { obj; _ } -> expression_free_vars obj
  | Call { callee; args } ->
    callee :: List.concat_map expression_free_vars args
  | CallPtr { callee; args } ->
    expression_free_vars callee @ List.concat_map expression_free_vars args
  | Unary { arg; _ } | Cast { arg; _ } | Length arg | HasTag { arg; _ } ->
    expression_free_vars arg
  | Binary { left; right; _ } | Logical { left; right; _ } ->
    expression_free_vars left @ expression_free_vars right
  | Cond { test; cons; alt } ->
    expression_free_vars test @ expression_free_vars cons @ expression_free_vars alt
  | AllocArray { size; _ } -> expression_free_vars size

(* Check that free locals are defined; return the free functions. *)
let check_expression_uses (locals : SSet.t) (defined : SSet.t) (exp : expr) : ident list =
  List.filter
    (fun (x : ident) ->
      if SSet.mem x.name locals then begin
        if not (SSet.mem x.name defined) then
          Err.type_error ?loc:(Some exp.eloc)
            (Printf.sprintf "local '%s' used without necessarily being defined" x.name);
        false
      end
      else true)
    (expression_free_vars exp)

type result = {
  locals : SSet.t; (* locals valid after this statement *)
  defined : SSet.t; (* locals definitely defined after this statement *)
  functions : ident list; (* free functions used in this statement *)
  returns : bool; (* does this statement return on every path? *)
}

let empty_block : stmt =
  { s = Block { body = []; block_env = None }; sloc = Loc.mk { line = 1; col = 1 } { line = 1; col = 1 } }

let rec check_stmt_flow (locals : SSet.t) (constants : SSet.t) (defined : SSet.t) (stm : stmt) :
    result =
  match stm.s with
  | Assign { op; lhs; rhs } -> (
    let functions = check_expression_uses locals defined rhs in
    match (op, lhs.e) with
    | AEq, Var id ->
      if SSet.mem id.name constants then
        Err.type_error ~loc:stm.sloc
          (Printf.sprintf "assigning to %s is not permitted when %s is used in postcondition"
             id.name id.name);
      { locals; defined = SSet.add id.name defined; functions; returns = false }
    | _ ->
      let more = check_expression_uses locals defined lhs in
      { locals; defined; functions = functions @ more; returns = false })
  | Update { arg; _ } ->
    { locals; defined; functions = check_expression_uses locals defined arg; returns = false }
  | ExprStmt e ->
    { locals; defined; functions = check_expression_uses locals defined e; returns = false }
  | VarDecl { id; init; _ } -> (
    match init with
    | None -> { locals = SSet.add id.name locals; defined; functions = []; returns = false }
    | Some e ->
      {
        locals = SSet.add id.name locals;
        defined = SSet.add id.name defined;
        functions = check_expression_uses locals defined e;
        returns = false;
      })
  | If { test; cons; alt } -> (
    let test_funcs = check_expression_uses locals defined test in
    let cons_r = check_stmt_flow locals constants defined cons in
    match alt with
    | Some alt ->
      let alt_r = check_stmt_flow locals constants defined alt in
      {
        locals;
        defined = SSet.inter cons_r.defined alt_r.defined;
        functions = test_funcs @ cons_r.functions @ alt_r.functions;
        returns = cons_r.returns && alt_r.returns;
      }
    | None ->
      { locals; defined; functions = test_funcs @ cons_r.functions; returns = false })
  | While { invariants; test; body } ->
    let funcs = ref (check_expression_uses locals defined test) in
    List.iter (fun e -> funcs := !funcs @ check_expression_uses locals defined e) invariants;
    let body_r = check_stmt_flow locals constants defined body in
    { locals; defined; functions = !funcs @ body_r.functions; returns = false }
  | For { invariants; init; test; update; body } ->
    let init_r =
      check_stmt_flow locals constants defined (Option.value init ~default:empty_block)
    in
    let funcs = ref init_r.functions in
    funcs := !funcs @ check_expression_uses init_r.locals init_r.defined test;
    List.iter
      (fun e -> funcs := !funcs @ check_expression_uses init_r.locals init_r.defined e)
      invariants;
    let body_r = check_stmt_flow init_r.locals constants init_r.defined body in
    let update_r =
      check_stmt_flow init_r.locals constants body_r.defined
        (Option.value update ~default:empty_block)
    in
    {
      locals;
      defined = init_r.defined;
      functions = !funcs @ body_r.functions @ update_r.functions;
      returns = false;
    }
  | Return arg ->
    {
      locals;
      defined = locals; (* everything counts as defined after a return *)
      functions =
        (match arg with None -> [] | Some e -> check_expression_uses locals defined e);
      returns = true;
    }
  | Block block ->
    let funcs = ref [] in
    let final =
      List.fold_left
        (fun (locals', defined', returns') stm ->
          let r = check_stmt_flow locals' constants defined' stm in
          funcs := !funcs @ r.functions;
          (r.locals, r.defined, returns' || r.returns))
        (locals, defined, false) block.body
    in
    let _, final_defined, returns = final in
    {
      locals;
      defined = SSet.filter (fun x -> SSet.mem x locals) final_defined;
      functions = !funcs;
      returns;
    }
  | Assert { test; _ } ->
    { locals; defined; functions = check_expression_uses locals defined test; returns = false }
  | ErrorStmt arg ->
    { locals; defined; functions = check_expression_uses locals defined arg; returns = true }
  | Break | Continue -> { locals; defined = locals; functions = []; returns = false }
