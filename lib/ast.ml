(* C1 abstract syntax, closely following the reference implementation's ast.ts
   (which itself follows the C0 reference grammar). All nodes carry source
   spans so the server can answer position-based queries.

   A few fields are mutable because the typechecker decorates the tree:
   - [Field.struct_name] records which struct a field access resolved to
   - [block.block_env] records the variable environment of each block
   - [fundecl.is_local_to] marks non-interface functions from object files *)

module SMap = Map.Make (String)

type ident = { name : string; iloc : Loc.span option }

type typ = { t : typ_d; tloc : Loc.span option }

and typ_d =
  | Int
  | Bool
  | String
  | Char
  | Void
  | Pointer of typ
  | Array of typ
  | StructT of ident
  | Named of ident (* a typedef'd name *)

type unop = UNot | UBitNot | UNeg | UDeref | UAddrOf
type binop = Times | Div | Mod | Plus | Minus | Shl | Shr | Lt | Le | Ge | Gt | Eq | Neq | BAnd | BXor | BOr
type logop = LAnd | LOr

type asnop =
  | AEq | APlus | AMinus | ATimes | ADiv | AMod
  | AShl | AShr | ABAnd | ABXor | ABOr

type expr = { e : expr_d; eloc : Loc.span }

and expr_d =
  | Var of ident
  | IntLit of { value : int32; raw : string }
  | StrLit of { value : string; raw : string }
  | ChrLit of { value : string; raw : string }
  | BoolLit of bool
  | Null
  | Index of { obj : expr; index : expr }
  | Field of field_access
  | Call of { callee : ident; args : expr list }
  | CallPtr of { callee : expr; args : expr list }
  | Cast of { kind : typ; arg : expr }
  | Unary of { op : unop; arg : expr }
  | Binary of { op : binop; left : expr; right : expr }
  | Logical of { op : logop; left : expr; right : expr }
  | Cond of { test : expr; cons : expr; alt : expr }
  | Alloc of typ
  | AllocArray of { kind : typ; size : expr }
  | Result
  | Length of expr
  | HasTag of { kind : typ; arg : expr }

and field_access = {
  deref : bool; (* e->f vs e.f *)
  obj : expr;
  field : ident;
  mutable struct_name : string option; (* filled in by the typechecker *)
}

(* Variable environments, recorded on blocks by the typechecker and used to
   answer hover/completion queries. *)
type env_entry = { ty : typ; def_loc : Loc.span option }
type venv = env_entry SMap.t

type stmt = { s : stmt_d; sloc : Loc.span }

and stmt_d =
  | Assign of { op : asnop; lhs : expr; rhs : expr }
  | Update of { op : [ `Incr | `Decr ]; arg : expr }
  | ExprStmt of expr
  | VarDecl of { kind : typ; id : ident; init : expr option }
  | If of { test : expr; cons : stmt; alt : stmt option }
  | While of { invariants : expr list; test : expr; body : stmt }
  | For of {
      invariants : expr list;
      init : stmt option; (* simple statement or variable declaration *)
      test : expr;
      update : stmt option; (* simple statement *)
      body : stmt;
    }
  | Return of expr option
  | Block of block
  | Assert of { contract : bool; test : expr }
  | ErrorStmt of expr
  | Break
  | Continue

and block = { body : stmt list; mutable block_env : venv option }

(* A struct field, function parameter, or typedef body: a type and a name. *)
type param = { p_kind : typ; p_id : ident; p_loc : Loc.span option }

type fundecl = {
  returns : typ;
  fname : ident;
  params : param list;
  preconds : expr list;
  postconds : expr list;
  fbody : stmt option; (* always a Block when present *)
  fdoc : string;
  floc : Loc.span option;
  (* Set for functions from object files that are not part of the file's
     interface section; calling them from another file is a warning. *)
  mutable is_local_to : string option;
}

type structdecl = {
  s_id : ident;
  s_fields : param list option; (* None = declared but not defined *)
  s_doc : string;
  s_loc : Loc.span option;
}

type decl =
  | FunDecl of fundecl
  | StructDecl of structdecl
  | TypeDef of { td_def : param; td_doc : string; td_loc : Loc.span option }
  | FunTypeDef of fundecl
  | UseLib of { lib : string; ul_loc : Loc.span option }
  | UseFile of { path : string; uf_loc : Loc.span option }

let decl_loc = function
  | FunDecl f -> f.floc
  | StructDecl s -> s.s_loc
  | TypeDef t -> t.td_loc
  | FunTypeDef f -> f.floc
  | UseLib u -> u.ul_loc
  | UseFile u -> u.uf_loc

let binop_string = function
  | Times -> "*" | Div -> "/" | Mod -> "%" | Plus -> "+" | Minus -> "-"
  | Shl -> "<<" | Shr -> ">>" | Lt -> "<" | Le -> "<=" | Ge -> ">=" | Gt -> ">"
  | Eq -> "==" | Neq -> "!=" | BAnd -> "&" | BXor -> "^" | BOr -> "|"

let logop_string = function LAnd -> "&&" | LOr -> "||"

let unop_string = function
  | UNot -> "!" | UBitNot -> "~" | UNeg -> "-" | UDeref -> "*" | UAddrOf -> "&"

let asnop_string = function
  | AEq -> "=" | APlus -> "+=" | AMinus -> "-=" | ATimes -> "*=" | ADiv -> "/="
  | AMod -> "%=" | AShl -> "<<=" | AShr -> ">>=" | ABAnd -> "&=" | ABXor -> "^=" | ABOr -> "|="
