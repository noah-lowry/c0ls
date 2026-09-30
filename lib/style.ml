(* Style warnings (port of style.ts):
   - if (c) { return true; } else { return false; }
   - comparisons with bool literals (x == true)
   - comparisons against int_max()/int_min() that are always true/false *)

open Ast

let warn ?loc msg hints : Err.terror =
  { Err.msg = Err.with_hints msg hints; tloc = loc; severity = Err.Warning }

let rec is_only_return_bool (stm : stmt) (b : bool) : bool =
  match stm.s with
  | Return (Some { e = BoolLit v; _ }) -> v = b
  | Block { body = [ inner ]; _ } -> is_only_return_bool inner b
  | _ -> false

let is_call_to (e : expr) (name : string) : bool =
  match e.e with Call { callee; _ } -> callee.name = name | _ -> false

let check_binary (out : Err.terror list ref) (e : expr) (op : binop) (left : expr) (right : expr) :
    unit =
  (match (left.e, right.e, op) with
  | (BoolLit _, _, (Eq | Neq)) | (_, BoolLit _, (Eq | Neq)) ->
    out :=
      warn ~loc:e.eloc
        (Printf.sprintf "unneeded %sequality comparison with bool literal"
           (if op = Neq then "in" else ""))
        [ "consider rewriting this to just use the non-literal operand, and perhaps negation" ]
      :: !out
  | _ -> ());
  let always_true =
    (op = Le && is_call_to right "int_max")
    || (op = Ge && is_call_to left "int_max")
    || (op = Ge && is_call_to right "int_min")
    || (op = Le && is_call_to left "int_min")
  in
  let always_false =
    (op = Gt && is_call_to right "int_max")
    || (op = Lt && is_call_to left "int_max")
    || (op = Lt && is_call_to right "int_min")
    || (op = Gt && is_call_to left "int_min")
  in
  if always_true then out := warn ~loc:e.eloc "this comparison is always true" [] :: !out;
  if always_false then out := warn ~loc:e.eloc "this comparison is always false" [] :: !out

let rec walk_expr (out : Err.terror list ref) (e : expr) : unit =
  match e.e with
  | Var _ | IntLit _ | StrLit _ | ChrLit _ | BoolLit _ | Null | Result -> ()
  | Index { obj; index } ->
    walk_expr out obj;
    walk_expr out index
  | Field { obj; _ } -> walk_expr out obj
  | Call { args; _ } -> List.iter (walk_expr out) args
  | CallPtr { callee; args } ->
    walk_expr out callee;
    List.iter (walk_expr out) args
  | Cast { arg; _ } | Unary { arg; _ } | Length arg | HasTag { arg; _ } -> walk_expr out arg
  | Binary { op; left; right } ->
    check_binary out e op left right;
    walk_expr out left;
    walk_expr out right
  | Logical { left; right; _ } ->
    walk_expr out left;
    walk_expr out right
  | Cond { test; cons; alt } ->
    walk_expr out test;
    walk_expr out cons;
    walk_expr out alt
  | Alloc _ -> ()
  | AllocArray { size; _ } -> walk_expr out size

let rec walk_stmt (out : Err.terror list ref) (stm : stmt) : unit =
  match stm.s with
  | Assign { lhs; rhs; _ } ->
    walk_expr out lhs;
    walk_expr out rhs
  | Update { arg; _ } -> walk_expr out arg
  | ExprStmt e -> walk_expr out e
  | VarDecl { init; _ } -> Option.iter (walk_expr out) init
  | If { test; cons; alt } ->
    (match alt with
    | Some alt_s ->
      if is_only_return_bool cons true && is_only_return_bool alt_s false then
        out :=
          warn ~loc:stm.sloc "unnecessary if statement"
            [ "consider replacing this if statement with 'return <loop guard>;'" ]
          :: !out
      else if is_only_return_bool cons false && is_only_return_bool alt_s true then
        out :=
          warn ~loc:stm.sloc "unnecessary if statement"
            [ "consider replacing this if statement with 'return !<loop guard>;'" ]
          :: !out
    | None -> ());
    walk_expr out test;
    walk_stmt out cons;
    Option.iter (walk_stmt out) alt
  | While { invariants; test; body } ->
    List.iter (walk_expr out) invariants;
    walk_expr out test;
    walk_stmt out body
  | For { invariants; init; test; update; body } ->
    List.iter (walk_expr out) invariants;
    Option.iter (walk_stmt out) init;
    walk_expr out test;
    Option.iter (walk_stmt out) update;
    walk_stmt out body
  | Return arg -> Option.iter (walk_expr out) arg
  | Block { body; _ } -> List.iter (walk_stmt out) body
  | Assert { test; _ } -> walk_expr out test
  | ErrorStmt e -> walk_expr out e
  | Break | Continue -> ()

let check_program (decls : decl list) : Err.terror list =
  let all = ref [] in
  List.iter
    (fun decl ->
      let out = ref [] in
      (match decl with
      | FunDecl f ->
        List.iter (walk_expr out) f.preconds;
        List.iter (walk_expr out) f.postconds;
        Option.iter (walk_stmt out) f.fbody
      | _ -> ());
      (* stamp sources so per-file filtering works *)
      let source = match decl_loc decl with Some l -> l.Loc.source | None -> None in
      List.iter
        (fun (te : Err.terror) ->
          (match (source, te.Err.tloc) with
          | Some src, Some tl when tl.Loc.source = None -> tl.Loc.source <- Some src
          | _ -> ());
          all := te :: !all)
        (List.rev !out))
    decls;
  List.rev !all
