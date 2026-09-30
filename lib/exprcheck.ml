(* Typechecking of expressions (bidirectional: synth + check), a direct port
   of typecheck/expressions.ts including its error messages. *)

open Ast
open Genv
open Typerel

(* Where is the expression being checked? Contracts permit \result, \length
   and \hastag; ordinary code does not. *)
type mode =
  | Ordinary
  | Requires
  | Ensures of typ (* function return type *)
  | LoopInvariant
  | AssertAnno

let mode_name = function
  | Ordinary -> "" (* not used in messages *)
  | Requires -> "@requires"
  | Ensures _ -> "@ensures"
  | LoopInvariant -> "@loop_invariant"
  | AssertAnno -> "@assert"

type ctx = {
  genv : Genv.t;
  source_file : string option; (* URI of the file being checked *)
}

let terr ?loc ?hints msg = Err.type_error ?loc ?hints msg

let rec synth (cx : ctx) (env : venv) (mode : mode) (exp : expr) : synthed =
  let genv = cx.genv in
  match exp.e with
  | Var id -> (
    match SMap.find_opt id.name env with
    | Some entry -> Ty entry.ty
    | None -> terr ~loc:exp.eloc (Printf.sprintf "variable %s not declared" id.name))
  | IntLit _ -> Ty { t = Int; tloc = None }
  | StrLit _ -> Ty { t = String; tloc = None }
  | ChrLit _ -> Ty { t = Char; tloc = None }
  | BoolLit _ -> Ty { t = Bool; tloc = None }
  | Null -> AmbiguousNull
  | Index { obj; index } -> (
    let object_type = actual_synthed genv (synth cx env mode obj) in
    match object_type with
    | Act (AArray arg) ->
      check cx env mode index { t = Int; tloc = None };
      Ty arg
    | _ ->
      terr ~loc:exp.eloc
        (Printf.sprintf "subject of indexing '[...]' is %s, not an array"
           (value_desc genv object_type)))
  | Field f -> (
    let object_type = actual_synthed genv (synth cx env mode f.obj) in
    let struct_id =
      if f.deref then begin
        match object_type with
        | Act (AStruct _) ->
          terr ~loc:exp.eloc
            (Printf.sprintf "cannot dereference non-pointer struct with e->%s" f.field.name)
            ~hints:[ Printf.sprintf "try e.%s" f.field.name ]
        | Act (APointer arg) -> (
          match actual_type genv arg with
          | AStruct id -> id
          | inner ->
            terr ~loc:exp.eloc
              (Printf.sprintf "subject of dereference '->%s' is a pointer to %s, not a pointer to a struct"
                 f.field.name
                 (value_desc genv (Act inner))))
        | _ ->
          terr ~loc:exp.eloc
            (Printf.sprintf "subject of dereference '->%s' is %s, not a pointer to a struct"
               f.field.name (value_desc genv object_type))
      end
      else begin
        match object_type with
        | Act (AStruct id) -> id
        | _ ->
          terr ~loc:exp.eloc
            (Printf.sprintf "subject of access '.%s' is %s, not a struct" f.field.name
               (value_desc genv object_type))
      end
    in
    let accessor = if f.deref then "->" else "." in
    match get_struct_definition genv struct_id.name with
    | None ->
      terr ~loc:exp.eloc
        (Printf.sprintf "subject of access '%s%s' is 'struct %s', which is not defined" accessor
           f.field.name struct_id.name)
    | Some { s_fields = None; _ } ->
      terr ~loc:exp.eloc
        (Printf.sprintf "subject of access '%s%s' is 'struct %s', which is declared but not defined"
           accessor f.field.name struct_id.name)
    | Some ({ s_fields = Some fields; _ } as sd) -> (
      f.struct_name <- Some sd.s_id.name;
      match List.find_opt (fun fd -> fd.p_id.name = f.field.name) fields with
      | Some fd -> Ty fd.p_kind
      | None ->
        terr ~loc:exp.eloc
          (Printf.sprintf "field '%s' not declared in 'struct %s'" f.field.name struct_id.name)))
  | Call { callee; args } -> (
    if SMap.mem callee.name env then
      terr ~loc:exp.eloc
        (Printf.sprintf "local '%s' is %s, not a function" callee.name
           (value_desc genv
              (actual_synthed genv (Ty (SMap.find callee.name env).ty))));
    let fname = callee.name in
    if is_printf_like genv fname then synth_printf cx env mode exp fname args
    else
      match get_function_declaration genv fname with
      | None ->
        let function_names =
          List.filter_map
            (function FunDecl f -> Some f.fname.name | _ -> None)
            (Genv.decls genv)
        in
        let alternatives = Util.best_matches fname function_names in
        if alternatives = [] then
          terr ~loc:exp.eloc (Printf.sprintf "function %s not declared" fname)
        else
          terr ~loc:exp.eloc
            (Printf.sprintf "function %s not declared" fname)
            ~hints:
              [
                "perhaps you meant one of: "
                ^ String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") alternatives);
              ]
      | Some func ->
        (* interface check for functions from object files *)
        (match func.is_local_to with
        | Some file when Some file <> cx.source_file ->
          raise
            (Err.Type_error
               {
                 Err.msg =
                   Printf.sprintf "function %s is not part of the interface of %s" fname
                     (!Util.display_path file);
                 tloc = Some exp.eloc;
                 severity = Err.Warning;
               })
        | _ -> ());
        let nparams = List.length func.params in
        let nargs = List.length args in
        if nargs <> nparams then
          terr ~loc:exp.eloc
            (Printf.sprintf "function %s requires %d argument%s but was given %d" fname nparams
               (if nparams = 1 then "" else "s")
               nargs);
        List.iter2 (fun arg param -> check cx env mode arg param.p_kind) args func.params;
        Ty func.returns)
  | CallPtr { callee; args } -> (
    let call_type = synth cx env mode callee in
    match call_type with
    | AnonFunPtr _ ->
      terr ~loc:exp.eloc "function pointers must be stored in locals before they are called"
    | AmbiguousNull -> terr ~loc:exp.eloc "cannot call 'NULL' as a function"
    | NamedFun f ->
      terr ~loc:exp.eloc
        (Printf.sprintf "Can only call pointers to functions, the function type '%s' is not a pointer"
           f.fname.name)
    | Ty ty -> (
      match actual_type genv ty with
      | APointer arg -> (
        match actual_type genv arg with
        | ANamedFun f ->
          let nparams = List.length f.params in
          let nargs = List.length args in
          if nargs <> nparams then
            terr ~loc:exp.eloc
              (Printf.sprintf "function pointer call requires %d argument%s but was given %d"
                 nparams
                 (if nparams = 1 then "" else "s")
                 nargs);
          List.iter2 (fun a param -> check cx env mode a param.p_kind) args f.params;
          Ty f.returns
        | _ -> terr ~loc:exp.eloc "only pointers to functions can be called")
      | _ -> terr ~loc:exp.eloc "only pointers to functions can be called"))
  | Cast { kind; arg } -> (
    let cast_type = actual_type genv kind in
    match cast_type with
    | APointer cast_arg -> (
      let argument_type = actual_synthed genv (synth cx env mode arg) in
      match argument_type with
      | SAmbiguousNull ->
        (* NULL cast always ok *)
        Ty kind
      | SNamedFun _ | SAnonFunPtr _ | Act (ANamedFun _) ->
        terr ~loc:exp.eloc "only function pointers with assigned types can be cast to 'void*'"
          ~hints:[ "assign to a variable and then cast to 'void*'" ]
      | Act (APointer parg) ->
        if cast_arg.t = Void then begin
          if parg.t = Void then
            terr ~loc:exp.eloc "Casting a 'void*' as a 'void*' not permitted";
          Ty kind
        end
        else if parg.t <> Void then
          terr ~loc:exp.eloc "only casts to or from 'void*' allowed"
        else Ty kind
      | other ->
        terr ~loc:exp.eloc
          (Printf.sprintf "casts must be pointer types, not %s" (value_desc genv other)))
    | other ->
      terr ~loc:exp.eloc
        (Printf.sprintf "casts must be pointer types, not %s" (value_desc genv (Act other))))
  | Unary { op; arg } -> (
    match op with
    | UNot ->
      check cx env mode arg { t = Bool; tloc = None };
      Ty { t = Bool; tloc = None }
    | UBitNot | UNeg ->
      check cx env mode arg { t = Int; tloc = None };
      Ty { t = Int; tloc = None }
    | UAddrOf -> (
      match arg.e with
      | Var id -> (
        match get_function_declaration genv id.name with
        | None -> terr ~loc:exp.eloc (Printf.sprintf "There is no function named %s" id.name)
        | Some definition ->
          if SMap.mem id.name env then
            terr ~loc:exp.eloc
              (Printf.sprintf
                 "cannot take the address of function %s when it is also the name of a local"
                 id.name);
          AnonFunPtr definition)
      | _ ->
        terr ~loc:exp.eloc "address-of operation '&' can only be applied directly to a function name")
    | UDeref -> (
      let pointer_type = actual_synthed genv (synth cx env mode arg) in
      match pointer_type with
      | SAmbiguousNull -> terr ~loc:exp.eloc "cannot dereference 'NULL'"
      | SAnonFunPtr _ ->
        terr ~loc:exp.eloc "cannot dereference a function pointer immediately"
          ~hints:[ "assign it to a local first" ]
      | Act (APointer parg) ->
        if parg.t = Void then
          terr ~loc:exp.eloc "cannot dereference value of type 'void*'"
            ~hints:[ "cast to another pointer type with '(t*)'" ]
        else Ty parg
      | other ->
        terr ~loc:exp.eloc
          (Printf.sprintf "only pointers can be dereferenced, this is %s" (value_desc genv other))))
  | Binary { op; left; right } -> (
    match op with
    | Times | Div | Mod | Plus | Minus | Shl | Shr | BAnd | BXor | BOr ->
      check cx env mode left { t = Int; tloc = None };
      check cx env mode right { t = Int; tloc = None };
      Ty { t = Int; tloc = None }
    | Lt | Le | Ge | Gt -> (
      let left_type = actual_synthed genv (synth cx env mode left) in
      match left_type with
      | Act AInt ->
        check cx env mode right { t = Int; tloc = None };
        Ty { t = Bool; tloc = None }
      | Act AChar ->
        check cx env mode right { t = Char; tloc = None };
        Ty { t = Bool; tloc = None }
      | Act AString ->
        terr ~loc:exp.eloc
          (Printf.sprintf "cannot compare strings with '%s'" (binop_string op))
          ~hints:[ "use the 'string_compare' function from the library <string>" ]
      | other ->
        terr ~loc:exp.eloc
          (Printf.sprintf "cannot compare %s with '%s'" (value_desc genv other) (binop_string op))
          ~hints:
            [
              Printf.sprintf "only values of type 'int' and 'char' can be used with '%s'"
                (binop_string op);
            ])
    | Eq | Neq ->
      let left_t = synth cx env mode left in
      let right_t = synth cx env mode right in
      let lub = lub_small genv exp left_t right_t ~cond:false in
      (match lub with
      | Act AString ->
        terr ~loc:exp.eloc
          (Printf.sprintf "cannot compare strings with '%s'" (binop_string op))
          ~hints:[ "use the 'string_equal' function from the library <string>" ]
      | _ -> ());
      Ty { t = Bool; tloc = None })
  | Logical { op; left; right } -> (
    let ln = match op with LAnd -> "and" | LOr -> "or" in
    let left_type = actual_synthed genv (synth cx env mode left) in
    match left_type with
    | Act AInt ->
      terr ~loc:exp.eloc
        (Printf.sprintf "cannot perform logical-%s on integers" ln)
        ~hints:
          [
            Printf.sprintf "use the bitwise-%s operation %s instead?" ln
              (match op with LAnd -> "&" | LOr -> "|");
          ]
    | Act ABool ->
      check cx env mode right { t = Bool; tloc = None };
      Ty { t = Bool; tloc = None }
    | other ->
      terr ~loc:exp.eloc
        (Printf.sprintf "cannot perform logical-%s on type %s" ln (value_desc_type genv other)))
  | Cond { test; cons; alt } ->
    check cx env mode test { t = Bool; tloc = None };
    let left = synth cx env mode cons in
    let right = synth cx env mode alt in
    let lub = lub_small genv exp left right ~cond:true in
    synthed_of_actual genv lub left right
  | Alloc kind ->
    check_allocatable cx exp kind;
    Ty { t = Pointer kind; tloc = None }
  | AllocArray { kind; size } ->
    check_allocatable cx exp kind;
    check cx env mode size { t = Int; tloc = None };
    Ty { t = Array kind; tloc = None }
  | Result -> (
    match mode with
    | Ordinary ->
      terr ~loc:exp.eloc "\\result illegal in ordinary expressions"
        ~hints:[ "use only in @ensures annotations" ]
    | Ensures returns ->
      if returns.t = Void then
        terr ~loc:exp.eloc "\\result illegal in functions that return 'void'"
      else Ty returns
    | m ->
      terr ~loc:exp.eloc
        (Printf.sprintf "\\result illegal in %s annotations" (mode_name m))
        ~hints:[ "use only in @ensures annotations" ])
  | Length arg -> (
    if mode = Ordinary then
      terr ~loc:exp.eloc "\\length illegal in ordinary expressions"
        ~hints:[ "use only in annotations" ];
    match actual_synthed genv (synth cx env mode arg) with
    | Act (AArray _) -> Ty { t = Int; tloc = None }
    | other ->
      terr ~loc:exp.eloc
        (Printf.sprintf "argument to \\length is %s not an array" (value_desc genv other)))
  | HasTag { kind; arg } -> (
    if mode = Ordinary then
      terr ~loc:exp.eloc "\\hastag illegal in ordinary expressions"
        ~hints:[ "use only in annotations" ];
    match actual_type genv kind with
    | APointer parg ->
      if parg.t = Void then terr ~loc:exp.eloc "tag cannot be 'void*'";
      check cx env mode arg
        { t = Pointer { t = Void; tloc = None }; tloc = None };
      Ty { t = Bool; tloc = None }
    | other ->
      terr ~loc:exp.eloc
        (Printf.sprintf "type argument to \\hastag is %s, but must be a pointer"
           (value_desc genv (Act other)))
        ~hints:[ Printf.sprintf "try '\\hastag(%s*, ...)'" (value_desc_type genv (Act other)) ])

and value_desc (_genv : Genv.t) (s : actual_synthed) : string =
  match s with
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

and value_desc_type (genv : Genv.t) (s : actual_synthed) : string =
  (* the printed type, for messages that want a type rather than "a struct" *)
  ignore genv;
  match s with
  | Act AInt -> "int"
  | Act ABool -> "bool"
  | Act AString -> "string"
  | Act AChar -> "char"
  | Act AVoid -> "void"
  | Act (APointer arg) -> Print.typ { t = Pointer arg; tloc = None }
  | Act (AArray arg) -> Print.typ { t = Array arg; tloc = None }
  | Act (AStruct id) -> "struct " ^ id.name
  | Act (ANamedFun f) -> f.fname.name
  | SAmbiguousNull -> "null pointer"
  | SAnonFunPtr f -> Print.anon_fun_ptr_type f
  | SNamedFun f -> Print.named_fun_type f

and synthed_of_actual (genv : Genv.t) (lub : actual_synthed) (left : synthed) (right : synthed) :
    synthed =
  (* Reconstruct a synthed from the lub computation: prefer the original
     synthed forms so typedef names survive in hover output. *)
  ignore genv;
  ignore lub;
  (* The lub itself was computed from left/right; re-run the precise lub. *)
  match Typerel.least_upper_bound_synthed genv left right with
  | Some s -> s
  | None -> left (* unreachable: lub_small already raised on None *)

and check_allocatable (cx : ctx) (exp : expr) (kind : typ) : unit =
  let genv = cx.genv in
  match actual_type genv kind with
  | ANamedFun _ -> terr ~loc:exp.eloc "cannot allocate functions"
  | AStruct id -> (
    match get_struct_definition genv id.name with
    | Some { s_fields = Some _; _ } -> ()
    | _ ->
      terr ~loc:exp.eloc "cannot allocate struct that has not been defined"
        ~hints:[ Printf.sprintf "give a definition for 'struct %s'" id.name ])
  | _ -> ()

and synth_printf (cx : ctx) (env : venv) (mode : mode) (exp : expr) (fname : string)
    (args : expr list) : synthed =
  (match args with
  | [] -> terr ~loc:exp.eloc (Printf.sprintf "%s requires at least 1 argument" fname)
  | fmt :: rest -> (
    match fmt.e with
    | StrLit { value; _ } ->
      let specifiers = ref [] in
      let n = String.length value in
      let i = ref 0 in
      while !i < n do
        if value.[!i] = '%' then begin
          if !i + 1 = n then
            terr ~loc:fmt.eloc
              "'%' must be followed by a format specifier.\nTry '%%' to print a percent sign";
          let c2 = value.[!i + 1] in
          if c2 <> '%' then specifiers := Printf.sprintf "%%%c" c2 :: !specifiers;
          incr i
        end;
        incr i
      done;
      let specifiers = List.rev !specifiers in
      if List.length rest <> List.length specifiers then
        terr ~loc:exp.eloc
          (Printf.sprintf "found %d format specifiers, but got %d arguments"
             (List.length specifiers) (List.length rest));
      List.iter2
        (fun arg specifier ->
          match specifier with
          | "%d" -> check cx env mode arg { t = Int; tloc = None }
          | "%s" -> check cx env mode arg { t = String; tloc = None }
          | "%c" -> check cx env mode arg { t = Char; tloc = None }
          | _ ->
            terr ~loc:fmt.eloc
              (Printf.sprintf
                 "invalid format specifier '%s'. options are %%d, %%s, %%c, or %%%% for a literal %% sign"
                 specifier))
        rest specifiers
    | _ -> terr ~loc:fmt.eloc "argument must be a string constant"));
  Ty { t = (if fname = "printf" then Void else String); tloc = None }

and lub_small (genv : Genv.t) (exp : expr) (t1 : synthed) (t2 : synthed) ~(cond : bool) :
    actual_synthed =
  let do_that_thing_to () =
    if cond then "use the conditional expression 'e ? e1 : e2' on" else "check equality of"
  in
  match Typerel.least_upper_bound_synthed genv t1 t2 with
  | None ->
    terr ~loc:exp.eloc
      (Printf.sprintf "cannot %s expressions with different types \n  %s has type %s\n  %s has type %s"
         (do_that_thing_to ())
         (if cond then "first branch" else "left-hand side")
         (synthed_string t1)
         (if cond then "second branch" else "right-hand side")
         (synthed_string t2))
  | Some lub -> (
    match actual_synthed genv lub with
    | SNamedFun _ | Act (ANamedFun _) ->
      terr ~loc:exp.eloc
        (Printf.sprintf "cannot %s functions" (do_that_thing_to ()))
        ~hints:[ "use pointers to functions" ]
    | Act (AStruct _) ->
      terr ~loc:exp.eloc
        (Printf.sprintf "cannot %s structs" (do_that_thing_to ()))
        ~hints:[ "use pointers to structs" ]
    | Act AVoid ->
      terr ~loc:exp.eloc (Printf.sprintf "cannot %s expressions of type 'void'" (do_that_thing_to ()))
    | actual -> actual)

and check (cx : ctx) (env : venv) (mode : mode) (exp : expr) (tp : typ) : unit =
  let synthed = synth cx env mode exp in
  if not (is_subtype cx.genv synthed tp) then
    terr ~loc:exp.eloc
      (Printf.sprintf "expected to find a '%s', but this expression has an incompatible type: '%s'"
         (Print.typ tp) (synthed_string synthed))

(* Synthesize the type of an lvalue and require it to be assignable
   (small, not void, not a whole struct or function). *)
let synth_lvalue (cx : ctx) (env : venv) (exp : expr) : typ =
  let synthed = synth cx env Ordinary exp in
  match synthed with
  | AmbiguousNull | AnonFunPtr _ ->
    (* impossible: lvalue syntax excludes these *)
    terr ~loc:exp.eloc "this expression cannot be assigned to"
  | NamedFun f ->
    terr ~loc:exp.eloc
      (Printf.sprintf "cannot assign expression with function type %s" f.fname.name)
      ~hints:[ "use pointers to functions" ]
  | Ty ty -> (
    match ty.t with
    | Void -> terr ~loc:exp.eloc "cannot assign to an expression with type 'void'"
    | _ -> (
      match actual_type cx.genv ty with
      | AVoid -> terr ~loc:exp.eloc "cannot assign to an expression with type 'void'"
      | AStruct id ->
        terr ~loc:exp.eloc
          (Printf.sprintf "cannot assign expression with type 'struct %s'" id.name)
          ~hints:[ "Assign the parts of the struct individually" ]
      | ANamedFun f ->
        terr ~loc:exp.eloc
          (Printf.sprintf "cannot assign expression with function type %s" f.fname.name)
          ~hints:[ "use pointers to functions" ]
      | _ -> ty))
