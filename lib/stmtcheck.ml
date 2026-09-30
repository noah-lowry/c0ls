(* Typechecking of statements (port of typecheck/statements.ts). The
   variable environment is a persistent map threaded through the statement
   list; blocks record their final environment for use by hover/completion.

   Unlike the reference, every sub-check is wrapped so one error does not
   abort checking of the remaining statements. *)

open Ast
open Exprcheck

type errors = Err.terror list ref

let add_err (errors : errors) (te : Err.terror) = errors := te :: !errors

let catching (errors : errors) (f : unit -> unit) : unit =
  try f () with Err.Type_error te -> add_err errors te

let bool_t : typ = { t = Bool; tloc = None }
let int_t : typ = { t = Int; tloc = None }
let string_t : typ = { t = String; tloc = None }

let rec check_stmt (cx : ctx) (env : venv) (stm : stmt) ~(returning : typ option)
    ~(in_loop : bool) (errors : errors) : venv =
  match stm.s with
  | Assign { op; lhs; rhs } ->
    catching errors (fun () ->
        if op = AEq then begin
          let left = synth_lvalue cx env lhs in
          check cx env Ordinary rhs left
        end
        else begin
          check cx env Ordinary lhs int_t;
          check cx env Ordinary rhs int_t
        end);
    env
  | Update { arg; _ } ->
    catching errors (fun () -> check cx env Ordinary arg int_t);
    env
  | ExprStmt e ->
    catching errors (fun () ->
        match Typerel.actual_synthed cx.genv (synth cx env Ordinary e) with
        | Typerel.Act (Genv.AStruct id) ->
          add_err errors
            {
              Err.msg =
                Printf.sprintf "expressions used as statements cannot have type 'struct %s'" id.name;
              tloc = Some stm.sloc;
              severity = Err.Error;
            }
        | Typerel.Act (Genv.ANamedFun f) | Typerel.SNamedFun f ->
          add_err errors
            {
              Err.msg =
                Printf.sprintf "expressions used as statements cannot have function type '%s'"
                  f.fname.name;
              tloc = Some stm.sloc;
              severity = Err.Error;
            }
        | _ -> ());
    env
  | VarDecl { kind; id; init } -> (
    try
      Typerel.check_type_in_declaration cx.genv kind;
      if SMap.mem id.name env then begin
        add_err errors
          {
            Err.msg = Printf.sprintf "variable '%s' declared a second time" id.name;
            tloc = Some stm.sloc;
            severity = Err.Error;
          };
        env
      end
      else begin
        (match init with
        | Some e -> catching errors (fun () -> check cx env Ordinary e kind)
        | None -> ());
        SMap.add id.name { ty = kind; def_loc = id.iloc } env
      end
    with Err.Type_error te ->
      add_err errors te;
      env)
  | If { test; cons; alt } ->
    catching errors (fun () -> check cx env Ordinary test bool_t);
    ignore (check_stmt cx env cons ~returning ~in_loop errors);
    (match alt with
    | Some alt -> ignore (check_stmt cx env alt ~returning ~in_loop errors)
    | None -> ());
    env
  | While { invariants; test; body } ->
    catching errors (fun () -> check cx env Ordinary test bool_t);
    List.iter
      (fun anno -> catching errors (fun () -> check cx env LoopInvariant anno bool_t))
      invariants;
    ignore (check_stmt cx env body ~returning ~in_loop:true errors);
    env
  | For { invariants; init; test; update; body } ->
    let env0 =
      match init with
      | Some init -> check_stmt cx env init ~returning:None ~in_loop:false errors
      | None -> env
    in
    catching errors (fun () -> check cx env0 Ordinary test bool_t);
    (match update with
    | Some update -> ignore (check_stmt cx env0 update ~returning:None ~in_loop:false errors)
    | None -> ());
    List.iter
      (fun anno -> catching errors (fun () -> check cx env0 LoopInvariant anno bool_t))
      invariants;
    ignore (check_stmt cx env0 body ~returning ~in_loop:true errors);
    env
  | Return arg ->
    (match returning with
    | None ->
      add_err errors
        { Err.msg = "return statements not allowed"; tloc = Some stm.sloc; severity = Err.Error }
    | Some returns -> (
      match returns.t with
      | Void -> (
        match arg with
        | Some _ ->
          add_err errors
            {
              Err.msg = "function returning void must invoke 'return', not 'return e'";
              tloc = Some stm.sloc;
              severity = Err.Error;
            }
        | None -> ())
      | _ -> (
        match arg with
        | None ->
          add_err errors
            {
              Err.msg = Printf.sprintf "this function must return a %s" (Print.typ returns);
              tloc = Some stm.sloc;
              severity = Err.Error;
            }
        | Some e -> catching errors (fun () -> check cx env Ordinary e returns))));
    env
  | Block block ->
    let final_env =
      List.fold_left
        (fun env stm -> check_stmt cx env stm ~returning ~in_loop errors)
        env block.body
    in
    block.block_env <- Some final_env;
    env
  | Assert { contract; test } ->
    catching errors (fun () ->
        check cx env (if contract then AssertAnno else Ordinary) test bool_t);
    env
  | ErrorStmt arg ->
    catching errors (fun () -> check cx env Ordinary arg string_t);
    env
  | Break ->
    if not in_loop then
      add_err errors
        {
          Err.msg =
            Err.with_hints "break statement not allowed"
              [ "break statements must be inside the body of a for-loop or while-loop" ];
          tloc = Some stm.sloc;
          severity = Err.Error;
        };
    env
  | Continue ->
    if not in_loop then
      add_err errors
        {
          Err.msg =
            Err.with_hints "continue statement not allowed"
              [ "continue statements must be inside the body of a for-loop or while-loop" ];
          tloc = Some stm.sloc;
          severity = Err.Error;
        };
    env
