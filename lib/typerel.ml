(* Type relations: equality, least upper bounds, and subtyping, including the
   special types that only occur during synthesis (the type of NULL, function
   types, and pointers-to-functions created by &f). Mirrors types.ts. *)

open Ast
open Genv

(* The result of synthesizing an expression's type. *)
type synthed =
  | Ty of typ
  | AmbiguousNull (* type of NULL *)
  | NamedFun of fundecl (* a value of function type (via typedef) *)
  | AnonFunPtr of fundecl (* &f: pointer to a function, no name yet *)

(* [synthed] with the typedef names at the head resolved away. *)
type actual_synthed =
  | Act of actual
  | SAmbiguousNull
  | SNamedFun of fundecl
  | SAnonFunPtr of fundecl

let actual_synthed (genv : Genv.t) (s : synthed) : actual_synthed =
  match s with
  | Ty ty -> Act (actual_type genv ty)
  | AmbiguousNull -> SAmbiguousNull
  | NamedFun f -> SNamedFun f
  | AnonFunPtr f -> SAnonFunPtr f

let rec equal_types (genv : Genv.t) (t1 : typ) (t2 : typ) : bool =
  match (actual_type genv t1, actual_type genv t2) with
  | AInt, AInt | ABool, ABool | AString, AString | AChar, AChar | AVoid, AVoid -> true
  | APointer a, APointer b | AArray a, AArray b -> equal_types genv a b
  | AStruct a, AStruct b -> a.name = b.name
  | ANamedFun a, ANamedFun b -> a.fname.name = b.fname.name
  | _ -> false

let equal_function_types (genv : Genv.t) (d1 : fundecl) (d2 : fundecl) : bool =
  equal_types genv d1.returns d2.returns
  && List.length d1.params = List.length d2.params
  && List.for_all2 (fun p1 p2 -> equal_types genv p1.p_kind p2.p_kind) d1.params d2.params

let rec least_upper_bound_type (genv : Genv.t) (t1 : typ) (t2 : typ) : typ option =
  match (actual_type genv t1, actual_type genv t2) with
  | AInt, AInt | ABool, ABool | AString, AString | AChar, AChar | AVoid, AVoid -> Some t1
  | APointer a, APointer b -> (
    match least_upper_bound_type genv a b with
    | Some sub -> Some { t = Pointer sub; tloc = None }
    | None -> None)
  | AArray a, AArray b -> (
    match least_upper_bound_type genv a b with
    | Some sub -> Some { t = Array sub; tloc = None }
    | None -> None)
  | AStruct a, AStruct b -> if a.name = b.name then Some t1 else None
  | ANamedFun a, ANamedFun b -> if a.fname.name = b.fname.name then Some t1 else None
  | _ -> None

let is_pointer (genv : Genv.t) (t : typ) : bool =
  match actual_type genv t with APointer _ -> true | _ -> false

(* Least upper bound of two synthesized types; used for ==/!= and e ? e1 : e2. *)
let least_upper_bound_synthed (genv : Genv.t) (t1 : synthed) (t2 : synthed) : synthed option =
  match (t1, t2) with
  | AmbiguousNull, AmbiguousNull -> Some t1
  | AmbiguousNull, AnonFunPtr _ -> Some t2
  | AnonFunPtr _, AmbiguousNull -> Some t1
  | AmbiguousNull, NamedFun _ | NamedFun _, AmbiguousNull -> None
  | AmbiguousNull, Ty t -> if is_pointer genv t then Some t2 else None
  | Ty t, AmbiguousNull -> if is_pointer genv t then Some t1 else None
  | AnonFunPtr f1, AnonFunPtr f2 -> if equal_function_types genv f1 f2 then Some t1 else None
  | AnonFunPtr _, NamedFun _ | NamedFun _, AnonFunPtr _ -> None
  | AnonFunPtr f, Ty t | Ty t, AnonFunPtr f -> (
    match actual_type genv t with
    | APointer arg -> (
      match actual_type genv arg with
      | ANamedFun named -> if equal_function_types genv f named then Some (Ty t) else None
      | _ -> None)
    | _ -> None)
  | NamedFun f1, NamedFun f2 -> if f1.fname.name = f2.fname.name then Some t1 else None
  | NamedFun _, Ty _ | Ty _, NamedFun _ ->
    (* a Ty can also resolve to a named function type through a typedef *)
    let as_named s = match s with
      | NamedFun f -> Some f
      | Ty t -> ( match actual_type genv t with ANamedFun f -> Some f | _ -> None)
      | _ -> None
    in
    (match (as_named t1, as_named t2) with
    | Some f1, Some f2 when f1.fname.name = f2.fname.name -> Some t1
    | _ -> None)
  | Ty a, Ty b -> (
    match least_upper_bound_type genv a b with Some t -> Some (Ty t) | None -> None)

(* abstract <: concrete *)
let rec is_subtype (genv : Genv.t) (abstract : synthed) (concrete : typ) : bool =
  let actual_concrete = actual_type genv concrete in
  match actual_synthed genv abstract with
  | Act AInt -> actual_concrete = AInt
  | Act ABool -> actual_concrete = ABool
  | Act AString -> actual_concrete = AString
  | Act AChar -> actual_concrete = AChar
  | Act AVoid -> actual_concrete = AVoid
  | Act (APointer arg) -> (
    match actual_concrete with
    | APointer carg -> is_subtype genv (Ty arg) carg
    | _ -> false)
  | Act (AArray arg) -> (
    match actual_concrete with
    | AArray carg -> is_subtype genv (Ty arg) carg
    | _ -> false)
  | Act (AStruct id) -> (
    match actual_concrete with AStruct cid -> id.name = cid.name | _ -> false)
  | Act (ANamedFun f) | SNamedFun f -> (
    match actual_concrete with ANamedFun cf -> f.fname.name = cf.fname.name | _ -> false)
  | SAmbiguousNull -> ( match actual_concrete with APointer _ -> true | _ -> false)
  | SAnonFunPtr f -> (
    match actual_concrete with
    | APointer carg -> (
      match actual_type genv carg with
      | ANamedFun cf -> equal_function_types genv f cf
      | _ -> false)
    | _ -> false)

(* A type is invalid if it is void or contains void other than under a
   pointer (void* is a real type in C1). *)
let rec type_is_not_void (genv : Genv.t) (ty : typ) : bool =
  match actual_type genv ty with
  | AVoid -> false
  | APointer arg -> if arg.t = Void then true else type_is_not_void genv arg
  | AArray arg -> type_is_not_void genv arg
  | AInt | ABool | AString | AChar | AStruct _ | ANamedFun _ -> true

(* Local variables and function parameters must have small type. *)
let check_type_in_declaration ?(is_function_arg = false) (genv : Genv.t) (ty : typ) : unit =
  match actual_type genv ty with
  | AStruct id ->
    Err.type_error ?loc:ty.tloc
      (Printf.sprintf "type struct %s not small" id.name)
      ~hints:
        [
          (if is_function_arg then "cannot pass structs to or from functions; use pointers"
           else "cannot store structs as locals; use pointers");
        ]
  | ANamedFun f ->
    Err.type_error ?loc:ty.tloc
      (Printf.sprintf "Function type %s is not small" f.fname.name)
      ~hints:
        [
          (if is_function_arg then "cannot pass functions directly to or from functions; use pointers"
           else "cannot store functions as locals; store a function pointer");
        ]
  | _ ->
    if not (type_is_not_void genv ty) then
      Err.type_error ?loc:ty.tloc "type uses 'void' incorrectly"

let check_function_return_type (genv : Genv.t) (ty : typ) : unit =
  match ty.t with
  | Void -> ()
  | _ -> check_type_in_declaration ~is_function_arg:true genv ty

let synthed_string (s : synthed) : string =
  match s with
  | Ty t -> Print.typ t
  | AmbiguousNull -> "null pointer"
  | NamedFun f -> Print.named_fun_type f
  | AnonFunPtr f -> Print.anon_fun_ptr_type f

(* "an integer", "a struct", ... for error messages *)
let value_description (genv : Genv.t) (s : synthed) : string =
  match actual_synthed genv s with
  | Act AInt -> "an integer"
  | Act ABool -> "a boolean"
  | Act AString -> "a string"
  | Act AChar -> "a character"
  | Act AVoid -> "a void expression"
  | Act (APointer _) -> "a pointer"
  | Act (AArray _) -> "an array"
  | Act (AStruct _) -> "a struct"
  | Act (ANamedFun _) -> "a function"
  | SAmbiguousNull -> "a pointer"
  | SAnonFunPtr _ -> "a pointer"
  | SNamedFun _ -> "a function"
