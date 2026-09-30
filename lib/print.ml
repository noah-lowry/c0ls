(* Pretty-printing of types and expressions, used for diagnostics, hover
   text and completion documentation. Parentheses are inserted only where
   needed (mirroring print.ts). *)

open Ast

let rec typ (ty : typ) : string =
  match ty.t with
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Char -> "char"
  | Void -> "void"
  | Pointer arg -> typ arg ^ "*"
  | Array arg -> typ arg ^ "[]"
  | StructT id -> "struct " ^ id.name
  | Named id -> id.name

(* "int f(int x, int y)" — used for hover and completion docs *)
let fun_signature (f : fundecl) : string =
  Printf.sprintf "%s %s(%s)" (typ f.returns) f.fname.name
    (String.concat ", " (List.map (fun p -> typ p.p_kind ^ " " ^ p.p_id.name) f.params))

(* "(name(int,bool) => int)" — a named function type *)
let named_fun_type (f : fundecl) : string =
  Printf.sprintf "(%s(%s) => %s)" f.fname.name
    (String.concat "," (List.map (fun p -> typ p.p_kind) f.params))
    (typ f.returns)

(* "((int,bool) => int)*" — the type of &f *)
let anon_fun_ptr_type (f : fundecl) : string =
  Printf.sprintf "((%s) => %s)*"
    (String.concat "," (List.map (fun p -> typ p.p_kind) f.params))
    (typ f.returns)

(* operator precedence for printing; smaller binds tighter *)
let op_level (op : [ `B of binop | `L of logop ]) : int =
  match op with
  | `B (Times | Div | Mod) -> 1
  | `B (Plus | Minus) -> 2
  | `B (Shl | Shr) -> 3
  | `B (Lt | Gt | Le | Ge) -> 4
  | `B (Eq | Neq) -> 5
  | `B BAnd -> 6
  | `B BXor -> 7
  | `B BOr -> 8
  | `L _ -> 9 (* treat && and || as equal, always parenthesize mixtures *)

let parens s = "(" ^ s ^ ")"

let rec expr (e : expr) : string =
  match e.e with
  | Var id -> id.name
  | IntLit { raw; _ } -> raw
  | StrLit { raw; _ } -> raw
  | ChrLit { raw; _ } -> raw
  | BoolLit b -> string_of_bool b
  | Null -> "NULL"
  | Index { obj; index } -> Printf.sprintf "%s[%s]" (expr obj) (expr index)
  | Field { deref; obj; field; _ } ->
    let objs =
      match obj.e with
      | Binary _ | Logical _ | Cast _ | Cond _ | Unary _ -> parens (expr obj)
      | _ -> expr obj
    in
    Printf.sprintf "%s%s%s" objs (if deref then "->" else ".") field.name
  | Call { callee; args } ->
    Printf.sprintf "%s(%s)" callee.name (String.concat ", " (List.map expr args))
  | CallPtr { callee; args } ->
    Printf.sprintf "(*%s)(%s)" (expr callee) (String.concat ", " (List.map expr args))
  | Cast { kind; arg } ->
    let args =
      match arg.e with
      | Binary _ | Logical _ | Cond _ -> parens (expr arg)
      | _ -> expr arg
    in
    Printf.sprintf "(%s)%s" (typ kind) args
  | Unary { op; arg } ->
    let args =
      match arg.e with
      | Binary _ | Logical _ | Cond _ -> parens (expr arg)
      | _ -> expr arg
    in
    unop_string op ^ args
  | Binary { op; left; right } -> binaryish (`B op) left right
  | Logical { op; left; right } -> binaryish (`L op) left right
  | Cond { test; cons; alt } ->
    let br sub = match sub.e with Cond _ -> parens (expr sub) | _ -> expr sub in
    Printf.sprintf "%s ? %s : %s" (br test) (br cons) (br alt)
  | Alloc ty -> Printf.sprintf "alloc(%s)" (typ ty)
  | AllocArray { kind; size } -> Printf.sprintf "alloc_array(%s, %s)" (typ kind) (expr size)
  | Result -> "\\result"
  | Length arg -> Printf.sprintf "\\length(%s)" (expr arg)
  | HasTag { kind; arg } -> Printf.sprintf "\\hastag(%s, %s)" (typ kind) (expr arg)

and binaryish (op : [ `B of binop | `L of logop ]) (left : expr) (right : expr) : string =
  let level = op_level op in
  let op_str = match op with `B b -> binop_string b | `L l -> logop_string l in
  let left_str =
    match left.e with
    | Cond _ -> parens (expr left)
    | Binary { op = lop; _ } ->
      let ll = op_level (`B lop) in
      if ll > level then parens (expr left)
      else if ll = level && is_confusing (`B lop) op then parens (expr left)
      else expr left
    | Logical { op = lop; _ } ->
      let ll = op_level (`L lop) in
      if ll > level then parens (expr left)
      else if ll = level && is_confusing (`L lop) op then parens (expr left)
      else expr left
    | _ -> expr left
  in
  let right_str =
    match right.e with
    | Cond _ -> parens (expr right)
    | Binary { op = rop; _ } ->
      let rl = op_level (`B rop) in
      if rl > level then parens (expr right)
      else if rl = level && not (associative op (`B rop)) then parens (expr right)
      else expr right
    | Logical { op = rop; _ } ->
      let rl = op_level (`L rop) in
      if rl > level then parens (expr right)
      else if rl = level && not (associative op (`L rop)) then parens (expr right)
      else expr right
    | _ -> expr right
  in
  Printf.sprintf "%s %s %s" left_str op_str right_str

(* cases where redundant parens improve readability *)
and is_confusing (inner : [ `B of binop | `L of logop ]) (outer : [ `B of binop | `L of logop ]) : bool
    =
  match (inner, outer) with
  | `L LAnd, `L LOr | `L LOr, `L LAnd -> true
  | `B (Eq | Neq), `B (Eq | Neq) -> true
  | _ -> false

and associative (outer : [ `B of binop | `L of logop ]) (inner : [ `B of binop | `L of logop ]) :
    bool =
  match (outer, inner) with
  | `B Plus, `B Plus | `B Times, `B Times | `B BOr, `B BOr | `B BAnd, `B BAnd | `B BXor, `B BXor ->
    true
  | _ -> false
