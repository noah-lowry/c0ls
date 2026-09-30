(* The global environment: all declarations seen so far, plus bookkeeping
   about which of them came from libraries (mirrors globalenv.ts). *)

open Ast

module SSet = Set.Make (String)

type t = {
  mutable libstructs : SSet.t;
  mutable libfuncs : SSet.t;
  mutable libs_loaded : SSet.t; (* #use <foo> *)
  mutable files_loaded : SSet.t; (* URIs, from README.txt deps and #use "foo" *)
  decls : decl Dynarray.t;
}

let create () : t =
  {
    libstructs = SSet.empty;
    libfuncs = SSet.empty;
    libs_loaded = SSet.empty;
    files_loaded = SSet.empty;
    decls = Dynarray.create ();
  }

let decls (genv : t) : decl list = Dynarray.to_list genv.decls

let pop_last_decl (genv : t) : unit =
  if Dynarray.length genv.decls > 0 then ignore (Dynarray.pop_last genv.decls)

let add_decl ?(library = false) (genv : t) (decl : decl) : unit =
  Dynarray.add_last genv.decls decl;
  if library then begin
    match decl with
    | StructDecl s -> genv.libstructs <- SSet.add s.s_id.name genv.libstructs
    | FunDecl f -> genv.libfuncs <- SSet.add f.fname.name genv.libfuncs
    | _ -> ()
  end

let is_library_function (genv : t) (name : string) : bool = SSet.mem name genv.libfuncs
let is_library_struct (genv : t) (name : string) : bool = SSet.mem name genv.libstructs

(* printf (from <conio>) and format (from <string>) take a format string and
   a variable number of arguments; they are special-cased by the checker. *)
let is_printf_like (genv : t) (fname : string) : bool =
  (SSet.mem "conio" genv.libs_loaded && fname = "printf")
  || (SSet.mem "string" genv.libs_loaded && fname = "format")

(* A resolved (non-typedef-name) type. *)
type actual =
  | AInt
  | ABool
  | AString
  | AChar
  | AVoid
  | APointer of typ (* argument not yet resolved *)
  | AArray of typ
  | AStruct of ident
  | ANamedFun of fundecl (* a typedef'd function type *)

let find_decl (genv : t) (pred : decl -> 'a option) : 'a option =
  let n = Dynarray.length genv.decls in
  let rec go i = if i >= n then None else
    match pred (Dynarray.get genv.decls i) with
    | Some x -> Some x
    | None -> go (i + 1)
  in
  go 0

(* Look up what a typedef name stands for. *)
let get_typedef (genv : t) (name : string) : [ `Type of typ | `Fun of fundecl ] option =
  find_decl genv (function
    | TypeDef { td_def; _ } when td_def.p_id.name = name -> Some (`Type td_def.p_kind)
    | FunTypeDef f when f.fname.name = name -> Some (`Fun f)
    | _ -> None)

let get_typedef_decl (genv : t) (name : string) : decl option =
  find_decl genv (function
    | TypeDef { td_def; _ } as d when td_def.p_id.name = name -> Some d
    | FunTypeDef f as d when f.fname.name = name -> Some d
    | _ -> None)

(* Resolve typedef names in a type's head. *)
let rec actual_type (genv : t) (ty : typ) : actual =
  match ty.t with
  | Int -> AInt
  | Bool -> ABool
  | String -> AString
  | Char -> AChar
  | Void -> AVoid
  | Pointer arg -> APointer arg
  | Array arg -> AArray arg
  | StructT id -> AStruct id
  | Named id -> (
    match get_typedef genv id.name with
    | Some (`Type ty') -> actual_type genv ty'
    | Some (`Fun f) -> ANamedFun f
    | None ->
      (* should be impossible if parsing threaded typedefs correctly, but a
         stale cache can get us here; treat as an undefined struct-ish hole *)
      AStruct id)

(* The function definition for [name] if one exists, otherwise its latest
   declaration, otherwise None.

   When [filename] is given, prefer a definition from that file, and prefer a
   prototype over a definition from *another* file (so go-to-definition and
   hover stay within the user's view of the program). *)
let get_function_declaration ?filename (genv : t) (name : string) : fundecl option =
  let result = ref None in
  let found = ref None in
  let n = Dynarray.length genv.decls in
  (try
     for i = 0 to n - 1 do
       match Dynarray.get genv.decls i with
       | FunDecl f when f.fname.name = name -> (
         match filename with
         | Some file ->
           let source = match f.floc with Some l -> l.Loc.source | None -> None in
           if source = Some file && f.fbody <> None then begin
             found := Some f;
             raise Exit
           end;
           if source <> None && source <> Some file && f.fbody = None then begin
             found := Some f;
             raise Exit
           end;
           if !result = None then result := Some f
         | None ->
           if !result = None then result := Some f;
           if f.fbody <> None then begin
             found := Some f;
             raise Exit
           end)
       | _ -> ()
     done
   with Exit -> ());
  match !found with Some f -> Some f | None -> !result

(* The operative struct declaration: a definition if one exists. *)
let get_struct_definition (genv : t) (name : string) : structdecl option =
  let result = ref None in
  let n = Dynarray.length genv.decls in
  (try
     for i = 0 to n - 1 do
       match Dynarray.get genv.decls i with
       | StructDecl s when s.s_id.name = name ->
         if !result = None then result := Some s;
         if s.s_fields <> None then begin
           result := Some s;
           raise Exit
         end
       | _ -> ()
     done
   with Exit -> ());
  !result

let rec full_type_name (genv : t) (ty : typ) : string =
  match actual_type genv ty with
  | AVoid -> "void"
  | ABool -> "bool"
  | AInt -> "int"
  | AChar -> "char"
  | AString -> "string"
  | AStruct id -> "struct " ^ id.name
  | APointer arg -> full_type_name genv arg ^ "*"
  | AArray arg -> full_type_name genv arg ^ "[]"
  | ANamedFun f -> f.fname.name
