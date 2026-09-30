(* Recursive-descent parser for C1 (and the smaller languages L1-L4/C0).

   Where the reference implementation parses a loose superset with an Earley
   parser and then restricts it (restrictsyntax.ts), this parser folds the
   restriction into a single pass:
   - expressions are parsed permissively: assignments, e++/e--, assert() and
     error() parse as "statement expressions" ([sexp]) and are rejected with
     the reference's error messages when they appear in expression position;
   - language-level checks (features not present in L1..C0) fire as the
     corresponding node is built.

   Typedef feedback: the lexer emits plain identifiers; the parser keeps a
   set of typedef names and treats members of that set as type names. Names
   are registered as soon as a typedef's identifier is read, so later
   declarations (even on the same line) see them. *)

open Ast

type anno_kind = ARequires | AEnsures | ALoopInvariant | AAssert

let anno_name = function
  | ARequires -> "requires"
  | AEnsures -> "ensures"
  | ALoopInvariant -> "loop_invariant"
  | AAssert -> "assert"

type anno = { a_kind : anno_kind; a_test : expr; a_loc : Loc.span }

(* Result of expression parsing: either a real expression or a form that is
   only legal as a statement. *)
type sexp =
  | Ex of expr
  | SAssign of asnop * sexp * sexp * Loc.span
  | SUpdate of [ `Incr | `Decr ] * sexp * Loc.span
  | SAssertE of expr * Loc.span
  | SErrorE of expr * Loc.span

let sexp_loc = function
  | Ex e -> e.eloc
  | SAssign (_, _, _, l) | SUpdate (_, _, l) | SAssertE (_, l) | SErrorE (_, l) -> l

type t = {
  lx : Lexer.t;
  lang : Lang.t;
  typeids : (string, unit) Hashtbl.t;
  mutable buf : Lexer.token list;
  mutable diags : Err.diagnostic list;
  (* doc-comment capture (top level only) *)
  mutable capture : bool;
  mutable doc_buf : string list; (* lines, reversed *)
  mutable doc_block : bool;
  mutable doc_last_line : int;
}

let create ?(lang = Lang.C1) ?(typeids = []) (src : string) : t =
  let annos = match lang with Lang.C0 | Lang.C1 -> true | _ -> false in
  let tbl = Hashtbl.create 16 in
  List.iter (fun s -> Hashtbl.replace tbl s ()) typeids;
  {
    lx = Lexer.create ~annos src;
    lang;
    typeids = tbl;
    buf = [];
    diags = [];
    capture = false;
    doc_buf = [];
    doc_block = false;
    doc_last_line = -10;
  }

let type_ids (p : t) : string list = Hashtbl.fold (fun k () acc -> k :: acc) p.typeids []
let add_type_id (p : t) (name : string) = Hashtbl.replace p.typeids name ()
let is_type_id (p : t) (name : string) = Hashtbl.mem p.typeids name

(* ------------------------------------------------------------------ *)
(* Doc comments                                                        *)
(* ------------------------------------------------------------------ *)

let feed_comment (p : t) (text : string) (block : bool) (span : Loc.span) =
  let lines = String.split_on_char '\n' text in
  let adjacent =
    block = p.doc_block
    && span.start_p.line <= p.doc_last_line + 1
    && p.doc_buf <> []
  in
  if not adjacent then p.doc_buf <- [];
  p.doc_buf <- List.rev_append lines p.doc_buf;
  p.doc_block <- block;
  p.doc_last_line <- span.end_p.line

let trim_comment_line (line : string) : string =
  let n = String.length line in
  let start = ref 0 in
  while !start < n && (line.[!start] = ' ' || line.[!start] = '*' || line.[!start] = '\t') do incr start done;
  let stop = ref n in
  while !stop > !start
        && (line.[!stop - 1] = ' ' || line.[!stop - 1] = '*' || line.[!stop - 1] = '\t'
            || line.[!stop - 1] = '\r')
  do decr stop done;
  String.sub line !start (!stop - !start)

let take_doc (p : t) : string =
  let lines = List.rev_map trim_comment_line p.doc_buf in
  p.doc_buf <- [];
  match lines with
  | first :: _ when String.length first >= 7 && String.sub first 0 7 = "typedef" ->
    (* show "abstract typedefs" like `typedef _____ queue_t` as code *)
    String.concat "\n" (("`" ^ first ^ "`") :: List.tl lines)
  | _ -> String.concat "\n" lines

let clear_doc (p : t) = p.doc_buf <- []

(* ------------------------------------------------------------------ *)
(* Token stream                                                        *)
(* ------------------------------------------------------------------ *)

let rec fetch (p : t) : Lexer.token =
  let tok = Lexer.next p.lx in
  match tok.tok with
  | Lexer.TComment { text; block } ->
    if p.capture then feed_comment p text block tok.span;
    fetch p
  | _ -> tok

let peek (p : t) : Lexer.token =
  match p.buf with
  | t :: _ -> t
  | [] ->
    let t = fetch p in
    p.buf <- [ t ];
    t

let peek2 (p : t) : Lexer.token =
  match p.buf with
  | _ :: t :: _ -> t
  | [ t1 ] ->
    let t2 = fetch p in
    p.buf <- [ t1; t2 ];
    t2
  | [] ->
    let t1 = fetch p in
    let t2 = fetch p in
    p.buf <- [ t1; t2 ];
    t2

let advance (p : t) : Lexer.token =
  match p.buf with
  | t :: rest ->
    p.buf <- rest;
    t
  | [] -> fetch p

let err (span : Loc.span) fmt = Printf.ksprintf (fun m -> raise (Err.Parse_error (span, m))) fmt

let expect_sym (p : t) (s : string) : Lexer.token =
  let t = peek p in
  match t.tok with
  | Lexer.TSym s' when s' = s -> advance p
  | d -> err t.span "expected '%s' but found %s" s (Lexer.tok_string d)

let is_sym (p : t) (s : string) : bool =
  match (peek p).tok with Lexer.TSym s' -> s' = s | _ -> false

let is_kw (p : t) (s : string) : bool =
  match (peek p).tok with Lexer.TKw s' -> s' = s | _ -> false

let expect_kw (p : t) (s : string) : Lexer.token =
  let t = peek p in
  match t.tok with
  | Lexer.TKw s' when s' = s -> advance p
  | d -> err t.span "expected '%s' but found %s" s (Lexer.tok_string d)

(* An identifier usable as a variable/function/parameter name: typedef names
   are not allowed here (they lex as type identifiers in the reference). *)
let var_ident (p : t) : ident =
  let t = peek p in
  match t.tok with
  | Lexer.TIdent name when not (is_type_id p name) ->
    ignore (advance p);
    { name; iloc = Some t.span }
  | Lexer.TIdent name ->
    err t.span "'%s' is a type name and cannot be used as an identifier here" name
  | d -> err t.span "expected an identifier but found %s" (Lexer.tok_string d)

(* Struct names and field names may coincide with typedef names. *)
let any_ident (p : t) : ident =
  let t = peek p in
  match t.tok with
  | Lexer.TIdent name ->
    ignore (advance p);
    { name; iloc = Some t.span }
  | d -> err t.span "expected an identifier but found %s" (Lexer.tok_string d)

(* ------------------------------------------------------------------ *)
(* Language-level restriction                                          *)
(* ------------------------------------------------------------------ *)

let atleast (p : t) (span : Loc.span) (needed : Lang.t) (msg : string) : unit =
  if not (Lang.includes p.lang needed) then
    err span "%s not a part of the language '%s'" msg (Lang.to_string p.lang)

(* ------------------------------------------------------------------ *)
(* Types                                                               *)
(* ------------------------------------------------------------------ *)

let starts_type (p : t) : bool =
  match (peek p).tok with
  | Lexer.TKw ("int" | "bool" | "string" | "char" | "void" | "struct") -> true
  | Lexer.TIdent name -> is_type_id p name
  | _ -> false

let rec parse_type (p : t) : typ =
  let t = peek p in
  let base =
    match t.tok with
    | Lexer.TKw "int" ->
      ignore (advance p);
      { t = Int; tloc = Some t.span }
    | Lexer.TKw "bool" ->
      ignore (advance p);
      atleast p t.span Lang.L2 "type 'bool'";
      { t = Bool; tloc = Some t.span }
    | Lexer.TKw "string" ->
      ignore (advance p);
      atleast p t.span Lang.C0 "type 'string'";
      { t = String; tloc = Some t.span }
    | Lexer.TKw "char" ->
      ignore (advance p);
      atleast p t.span Lang.C0 "type 'char'";
      { t = Char; tloc = Some t.span }
    | Lexer.TKw "void" ->
      ignore (advance p);
      atleast p t.span Lang.L3 "type 'void'";
      { t = Void; tloc = Some t.span }
    | Lexer.TKw "struct" ->
      ignore (advance p);
      atleast p t.span Lang.L4 "struct types";
      let id = any_ident p in
      let end_span = match id.iloc with Some s -> s | None -> t.span in
      { t = StructT id; tloc = Some (Loc.span_of t.span end_span) }
    | Lexer.TIdent name when is_type_id p name ->
      ignore (advance p);
      atleast p t.span Lang.L3 "defined types";
      { t = Named { name; iloc = Some t.span }; tloc = Some t.span }
    | d -> err t.span "expected a type but found %s" (Lexer.tok_string d)
  in
  parse_type_postfix p base

and parse_type_postfix (p : t) (base : typ) : typ =
  let t = peek p in
  match t.tok with
  | Lexer.TSym "*" ->
    ignore (advance p);
    atleast p t.span Lang.L4 "pointer types";
    (if base.t = Void then atleast p t.span Lang.C1 "type 'void*'");
    let loc = match base.tloc with Some s -> Some (Loc.span_of s t.span) | None -> Some t.span in
    parse_type_postfix p { t = Pointer base; tloc = loc }
  | Lexer.TSym "[" -> (
    match (peek2 p).tok with
    | Lexer.TSym "]" ->
      ignore (advance p);
      let close = advance p in
      atleast p t.span Lang.L4 "array types";
      let loc = match base.tloc with Some s -> Some (Loc.span_of s close.span) | None -> Some close.span in
      parse_type_postfix p { t = Array base; tloc = loc }
    | _ -> base)
  | _ -> base

(* A value type: 'void' is only usable as a return type. *)
let parse_value_type (p : t) : typ =
  let ty = parse_type p in
  (match ty.t with
  | Void ->
    let span = match ty.tloc with Some s -> s | None -> (peek p).span in
    err span "Type 'void' can only be used as the return type of a function."
  | _ -> ());
  ty

let check_value_type (p : t) (ty : typ) : typ =
  match ty.t with
  | Void ->
    let span = match ty.tloc with Some s -> s | None -> (peek p).span in
    err span "Type 'void' can only be used as the return type of a function."
  | _ -> ty

(* ------------------------------------------------------------------ *)
(* Literals                                                            *)
(* ------------------------------------------------------------------ *)

let int_literal (span : Loc.span) (raw : string) : expr =
  let mk value = { e = IntLit { value; raw }; eloc = span } in
  if raw = "0" then mk 0l
  else if raw.[0] = '0' then begin
    (* must be a hex constant *)
    if String.length raw < 2 || Char.lowercase_ascii raw.[1] <> 'x' then
      err span
        "Bad numeric constant: %s\nIdentifiers beginning with '0' must be hex constants starting as '0X' or '0x'"
        raw;
    let digits = String.sub raw 2 (String.length raw - 2) in
    let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
    if digits = "" || not (String.for_all is_hex digits) then
      err span
        "Invalid hex constant: %s\nHex constants must only have the characters '0123456789abcdefABCDEF'"
        raw;
    (* strip leading zeros for the length check *)
    let stripped =
      let i = ref 0 in
      while !i < String.length digits - 1 && digits.[!i] = '0' do incr i done;
      String.sub digits !i (String.length digits - !i)
    in
    if String.length stripped > 8 then err span "Hex constant too large: %s" raw;
    let v = Int64.of_string ("0x" ^ stripped) in
    let v = if Int64.compare v 0x80000000L < 0 then v else Int64.sub v 0x100000000L in
    mk (Int64.to_int32 v)
  end
  else begin
    if not (String.for_all (fun c -> c >= '0' && c <= '9') raw) then
      err span "Invalid integer constant: %s" raw;
    if String.length raw > 10 then err span "Decimal constant too large: %s" raw;
    let v = Int64.of_string raw in
    if Int64.compare v 2147483648L > 0 then err span "Decimal constant too large: %s" raw;
    mk (if Int64.compare v 2147483648L < 0 then Int64.to_int32 v else Int32.min_int)
  end

let string_escapes = [ 'n'; 't'; 'v'; 'b'; 'r'; 'f'; 'a'; '\\'; '\''; '"' ]

let validate_string (span : Loc.span) (pieces : string list) : unit =
  List.iter
    (fun x ->
      if String.length x = 2 && x.[0] = '\\' then begin
        if not (List.mem x.[1] string_escapes) then
          err span "Invalid escape '%s' in string" x
      end
      else if not (String.for_all (fun c -> c >= ' ' && c <= '~') x) then
        err span "Invalid character in string '%s'" x)
    pieces

let char_literal (span : Loc.span) (raw : string) : expr =
  let mk value = { e = ChrLit { value; raw = Printf.sprintf "'%s'" raw }; eloc = span } in
  if String.length raw = 1 then begin
    if not (raw.[0] >= ' ' && raw.[0] <= '~') then err span "Invalid character '%s'" raw;
    mk raw
  end
  else if String.length raw = 2 && raw.[0] = '\\' then
    match raw.[1] with
    | 'n' -> mk "\n"
    | 't' -> mk "\t"
    | 'v' -> mk "\011"
    | 'b' -> mk "\b"
    | 'r' -> mk "\r"
    | 'f' -> mk "\012"
    | 'a' -> mk "a"
    | '\'' -> mk "'"
    | '"' -> mk "\""
    | '0' -> mk "\000"
    | '\\' -> mk "\\"
    | _ -> err span "Unexpected escape character '%s'" raw
  else err span "Invalid character literal '%s'" raw

(* ------------------------------------------------------------------ *)
(* Expressions                                                         *)
(* ------------------------------------------------------------------ *)

let to_expr (se : sexp) : expr =
  match se with
  | Ex e -> e
  | SAssign (op, l, _, span) ->
    ignore l;
    err span "Assignment 'x %s e2' must be used as a statement; it is used as an expression here."
      (asnop_string op)
  | SUpdate (op, _, span) ->
    err span "Increment/decrement operation 'e%s' must be used as a statement; it is used as an expression here."
      (match op with `Incr -> "++" | `Decr -> "--")
  | SAssertE (_, span) ->
    err span "The 'assert()' function must be used as a statement; it is used as an expression here."
  | SErrorE (_, span) ->
    err span "The 'error()' function must be used as a statement; it is used as an expression here."

let expr_desc (e : expr) : string =
  match e.e with
  | Var _ -> "identifier"
  | IntLit _ -> "integer literal"
  | StrLit _ -> "string literal"
  | ChrLit _ -> "character literal"
  | BoolLit _ -> "boolean literal"
  | Null -> "'NULL'"
  | Index _ -> "array access"
  | Field _ -> "struct field access"
  | Call _ -> "function call"
  | CallPtr _ -> "function pointer call"
  | Cast _ -> "cast"
  | Unary _ -> "unary operation"
  | Binary _ -> "binary operation"
  | Logical _ -> "logical operation"
  | Cond _ -> "conditional expression"
  | Alloc _ -> "allocation"
  | AllocArray _ -> "array allocation"
  | Result -> "'\\result'"
  | Length _ -> "'\\length'"
  | HasTag _ -> "'\\hastag'"

(* Validate that an expression has lvalue shape (restrictLValue). *)
let rec check_lvalue (p : t) (e : expr) : unit =
  match e.e with
  | Var _ -> ()
  | Field f ->
    atleast p e.eloc Lang.L4 "struct access";
    check_lvalue p f.obj
  | Index i ->
    atleast p e.eloc Lang.L4 "array access";
    check_lvalue p i.obj
  | Unary { op = UDeref; arg } -> (
    atleast p e.eloc Lang.L4 "pointer dereference";
    (* casts are allowed in the form "star (t star) e" *)
    match arg.e with
    | Cast { arg = inner; _ } -> check_lvalue p inner
    | _ -> check_lvalue p arg)
  | Unary { op; _ } ->
    err e.eloc "Unary %s operator not valid in lvalues" (unop_string op)
  | Cast _ ->
    err e.eloc "Casts on the left-side of an assignment must be of the form *(t*)e"
  | _ -> err e.eloc "a %s is not a valid LValue" (expr_desc e)

let to_lvalue (p : t) (se : sexp) : expr =
  let e = to_expr se in
  check_lvalue p e;
  e

let asnop_of_sym = function
  | "=" -> Some AEq
  | "+=" -> Some APlus
  | "-=" -> Some AMinus
  | "*=" -> Some ATimes
  | "/=" -> Some ADiv
  | "%=" -> Some AMod
  | "<<=" -> Some AShl
  | ">>=" -> Some AShr
  | "&=" -> Some ABAnd
  | "^=" -> Some ABXor
  | "|=" -> Some ABOr
  | _ -> None

(* binary/logical operator precedence: higher binds tighter *)
let binop_prec = function
  | "*" | "/" | "%" -> Some 10
  | "+" | "-" -> Some 9
  | "<<" | ">>" -> Some 8
  | "<" | "<=" | ">=" | ">" -> Some 7
  | "==" | "!=" -> Some 6
  | "&" -> Some 5
  | "^" -> Some 4
  | "|" -> Some 3
  | "&&" -> Some 2
  | "||" -> Some 1
  | _ -> None

let binop_of_sym = function
  | "*" -> Times | "/" -> Div | "%" -> Mod | "+" -> Plus | "-" -> Minus
  | "<<" -> Shl | ">>" -> Shr | "<" -> Lt | "<=" -> Le | ">=" -> Ge | ">" -> Gt
  | "==" -> Eq | "!=" -> Neq | "&" -> BAnd | "^" -> BXor | "|" -> BOr
  | s -> failwith ("binop_of_sym: " ^ s)

let rec parse_expr (p : t) : sexp =
  (* assignment level, right-associative *)
  let lhs = parse_cond p in
  let t = peek p in
  match t.tok with
  | Lexer.TSym s when asnop_of_sym s <> None ->
    let op = Option.get (asnop_of_sym s) in
    ignore (advance p);
    let rhs = parse_expr p in
    let span = Loc.span_of (sexp_loc lhs) (sexp_loc rhs) in
    SAssign (op, lhs, rhs, span)
  | _ -> lhs

and parse_cond (p : t) : sexp =
  let test = parse_binary p 1 in
  if is_sym p "?" then begin
    let test = to_expr test in
    atleast p test.eloc Lang.L2 "conditional expressions";
    ignore (advance p);
    let cons = to_expr (parse_expr p) in
    ignore (expect_sym p ":");
    let alt = to_expr (parse_cond p) in
    let span = Loc.span_of test.eloc alt.eloc in
    Ex { e = Cond { test; cons; alt }; eloc = span }
  end
  else test

and parse_binary (p : t) (min_prec : int) : sexp =
  let rec loop (lhs : sexp) : sexp =
    let t = peek p in
    match t.tok with
    | Lexer.TSym s -> (
      match binop_prec s with
      | Some prec when prec >= min_prec ->
        ignore (advance p);
        let left = to_expr lhs in
        let rhs = parse_binary p (prec + 1) in
        let right = to_expr rhs in
        let span = Loc.span_of left.eloc right.eloc in
        let node =
          if s = "&&" || s = "||" then begin
            atleast p span Lang.L2 (Printf.sprintf "logical operation '%s'" s);
            Logical { op = (if s = "&&" then LAnd else LOr); left; right }
          end
          else begin
            (match s with
            | "*" | "/" | "%" | "+" | "-" -> ()
            | _ -> atleast p span Lang.L2 (Printf.sprintf "binary operation '%s'" s));
            Binary { op = binop_of_sym s; left; right }
          end
        in
        loop (Ex { e = node; eloc = span })
      | _ -> lhs)
    | _ -> lhs
  in
  loop (parse_unary p)

and parse_unary (p : t) : sexp =
  let t = peek p in
  match t.tok with
  | Lexer.TSym "!" ->
    ignore (advance p);
    atleast p t.span Lang.L2 "boolean negation";
    let arg = to_expr (parse_unary p) in
    Ex { e = Unary { op = UNot; arg }; eloc = Loc.span_of t.span arg.eloc }
  | Lexer.TSym "~" ->
    ignore (advance p);
    let arg = to_expr (parse_unary p) in
    Ex { e = Unary { op = UBitNot; arg }; eloc = Loc.span_of t.span arg.eloc }
  | Lexer.TSym "-" ->
    ignore (advance p);
    let arg = to_expr (parse_unary p) in
    Ex { e = Unary { op = UNeg; arg }; eloc = Loc.span_of t.span arg.eloc }
  | Lexer.TSym "*" ->
    ignore (advance p);
    atleast p t.span Lang.L4 "pointer dereference";
    let arg = to_expr (parse_unary p) in
    Ex { e = Unary { op = UDeref; arg }; eloc = Loc.span_of t.span arg.eloc }
  | Lexer.TSym "&" ->
    ignore (advance p);
    atleast p t.span Lang.C1 "address-of";
    let arg = to_expr (parse_unary p) in
    Ex { e = Unary { op = UAddrOf; arg }; eloc = Loc.span_of t.span arg.eloc }
  | Lexer.TSym "(" when (match (peek2 p).tok with
                         | Lexer.TKw ("int" | "bool" | "string" | "char" | "void" | "struct") -> true
                         | Lexer.TIdent name -> is_type_id p name
                         | _ -> false) ->
    (* cast: (t)e *)
    ignore (advance p);
    let kind = parse_type p in
    ignore (expect_sym p ")");
    atleast p t.span Lang.C1 "casts";
    let kind = check_value_type p kind in
    let arg = to_expr (parse_unary p) in
    Ex { e = Cast { kind; arg }; eloc = Loc.span_of t.span arg.eloc }
  | _ -> parse_postfix p

and parse_postfix (p : t) : sexp =
  let rec loop (se : sexp) : sexp =
    let t = peek p in
    match t.tok with
    | Lexer.TSym "." | Lexer.TSym "->" ->
      let deref = (match t.tok with Lexer.TSym "->" -> true | _ -> false) in
      ignore (advance p);
      let obj = to_expr se in
      atleast p obj.eloc Lang.L4 "struct access";
      let field = any_ident p in
      let end_span = match field.iloc with Some s -> s | None -> t.span in
      let span = Loc.span_of obj.eloc end_span in
      loop (Ex { e = Field { deref; obj; field; struct_name = None }; eloc = span })
    | Lexer.TSym "[" ->
      ignore (advance p);
      let obj = to_expr se in
      atleast p obj.eloc Lang.L4 "array access";
      let index = to_expr (parse_expr p) in
      let close = expect_sym p "]" in
      loop (Ex { e = Index { obj; index }; eloc = Loc.span_of obj.eloc close.span })
    | Lexer.TSym "++" ->
      ignore (advance p);
      loop (SUpdate (`Incr, se, Loc.span_of (sexp_loc se) t.span))
    | Lexer.TSym "--" ->
      ignore (advance p);
      loop (SUpdate (`Decr, se, Loc.span_of (sexp_loc se) t.span))
    | _ -> se
  in
  loop (parse_primary p)

and parse_args (p : t) : expr list * Loc.span =
  (* caller consumed '('; returns the arguments and the ')' span *)
  if is_sym p ")" then begin
    let close = advance p in
    ([], close.span)
  end
  else begin
    let rec go acc =
      let arg = to_expr (parse_expr p) in
      if is_sym p "," then begin
        ignore (advance p);
        go (arg :: acc)
      end
      else begin
        let close = expect_sym p ")" in
        (List.rev (arg :: acc), close.span)
      end
    in
    go []
  end

and parse_primary (p : t) : sexp =
  let t = peek p in
  match t.tok with
  | Lexer.TSym "(" -> (
    ignore (advance p);
    let inner = parse_expr p in
    let close = expect_sym p ")" in
    (* "( star e ) ( args )" is an indirect function call *)
    match inner with
    | Ex { e = Unary { op = UDeref; arg }; _ } when is_sym p "(" ->
      ignore (advance p);
      atleast p t.span Lang.C1 "function pointer calls";
      let args, close2 = parse_args p in
      Ex { e = CallPtr { callee = arg; args }; eloc = Loc.span_of t.span close2 }
    | Ex e -> Ex { e = e.e; eloc = Loc.span_of t.span close.span }
    | se -> se)
  | Lexer.TNum raw ->
    ignore (advance p);
    Ex (int_literal t.span raw)
  | Lexer.TStr pieces ->
    ignore (advance p);
    atleast p t.span Lang.C0 "string literals";
    (* adjacent string literals concatenate *)
    let all = ref pieces in
    let last_span = ref t.span in
    let rec more () =
      match (peek p).tok with
      | Lexer.TStr more_pieces ->
        let tok = advance p in
        all := !all @ more_pieces;
        last_span := tok.span;
        more ()
      | _ -> ()
    in
    more ();
    let span = Loc.span_of t.span !last_span in
    validate_string span !all;
    let joined = String.concat "" !all in
    Ex { e = StrLit { value = joined; raw = "\"" ^ joined ^ "\"" }; eloc = span }
  | Lexer.TChr raw ->
    ignore (advance p);
    atleast p t.span Lang.C0 "character literals";
    Ex (char_literal t.span raw)
  | Lexer.TKw "true" ->
    ignore (advance p);
    atleast p t.span Lang.L2 "'true' and 'false'";
    Ex { e = BoolLit true; eloc = t.span }
  | Lexer.TKw "false" ->
    ignore (advance p);
    atleast p t.span Lang.L2 "'true' and 'false'";
    Ex { e = BoolLit false; eloc = t.span }
  | Lexer.TKw "NULL" ->
    ignore (advance p);
    atleast p t.span Lang.L4 "'NULL'";
    Ex { e = Null; eloc = t.span }
  | Lexer.TKw "alloc" ->
    ignore (advance p);
    atleast p t.span Lang.L4 "allocation";
    ignore (expect_sym p "(");
    let kind = parse_value_type p in
    let close = expect_sym p ")" in
    Ex { e = Alloc kind; eloc = Loc.span_of t.span close.span }
  | Lexer.TKw "alloc_array" ->
    ignore (advance p);
    atleast p t.span Lang.L4 "array allocation";
    ignore (expect_sym p "(");
    let kind = parse_value_type p in
    ignore (expect_sym p ",");
    let size = to_expr (parse_expr p) in
    let close = expect_sym p ")" in
    Ex { e = AllocArray { kind; size }; eloc = Loc.span_of t.span close.span }
  | Lexer.TKw "assert" ->
    ignore (advance p);
    ignore (expect_sym p "(");
    let test = to_expr (parse_expr p) in
    let close = expect_sym p ")" in
    SAssertE (test, Loc.span_of t.span close.span)
  | Lexer.TKw "error" ->
    ignore (advance p);
    ignore (expect_sym p "(");
    let arg = to_expr (parse_expr p) in
    let close = expect_sym p ")" in
    SErrorE (arg, Loc.span_of t.span close.span)
  | Lexer.TBackslash -> (
    ignore (advance p);
    let kw = peek p in
    match kw.tok with
    | Lexer.TIdent "result" ->
      ignore (advance p);
      atleast p t.span Lang.C0 "'\\result'";
      Ex { e = Result; eloc = Loc.span_of t.span kw.span }
    | Lexer.TIdent "length" ->
      ignore (advance p);
      atleast p t.span Lang.C0 "'\\length'";
      ignore (expect_sym p "(");
      let arg = to_expr (parse_expr p) in
      let close = expect_sym p ")" in
      Ex { e = Length arg; eloc = Loc.span_of t.span close.span }
    | Lexer.TIdent "hastag" ->
      ignore (advance p);
      atleast p t.span Lang.C1 "'\\hastag'";
      ignore (expect_sym p "(");
      let kind = parse_value_type p in
      ignore (expect_sym p ",");
      let arg = to_expr (parse_expr p) in
      let close = expect_sym p ")" in
      Ex { e = HasTag { kind; arg }; eloc = Loc.span_of t.span close.span }
    | d -> err kw.span "expected 'result', 'length', or 'hastag' after '\\' but found %s" (Lexer.tok_string d))
  | Lexer.TIdent name when not (is_type_id p name) -> (
    let id_tok = advance p in
    let id = { name; iloc = Some id_tok.span } in
    match (peek p).tok with
    | Lexer.TSym "(" ->
      ignore (advance p);
      atleast p t.span Lang.L3 "function calls";
      let args, close = parse_args p in
      Ex { e = Call { callee = id; args }; eloc = Loc.span_of t.span close }
    | _ -> Ex { e = Var id; eloc = id_tok.span })
  | Lexer.TIdent name ->
    err t.span "type name '%s' cannot be used as an expression" name
  | d -> err t.span "expected an expression but found %s" (Lexer.tok_string d)

(* ------------------------------------------------------------------ *)
(* Annotations                                                         *)
(* ------------------------------------------------------------------ *)

let parse_anno (p : t) : anno =
  let t = peek p in
  let kind =
    match t.tok with
    | Lexer.TKw "assert" -> AAssert
    | Lexer.TIdent "requires" -> ARequires
    | Lexer.TIdent "ensures" -> AEnsures
    | Lexer.TIdent "loop_invariant" -> ALoopInvariant
    | d ->
      err t.span
        "expected an annotation ('requires', 'ensures', 'loop_invariant', or 'assert') but found %s"
        (Lexer.tok_string d)
  in
  ignore (advance p);
  let test = to_expr (parse_expr p) in
  let semi = expect_sym p ";" in
  { a_kind = kind; a_test = test; a_loc = Loc.span_of t.span semi.span }

(* Parse zero or more annotation groups: /*@ ... @*/ or //@ ... \n *)
let rec parse_anno_groups (p : t) : anno list =
  let t = peek p in
  match t.tok with
  | Lexer.TAnnoStart ->
    ignore (advance p);
    let rec go acc =
      match (peek p).tok with
      | Lexer.TAnnoEnd ->
        ignore (advance p);
        List.rev acc
      | Lexer.TEOF ->
        err (peek p).span "expected '@*/' to close the annotation"
      | _ -> go (parse_anno p :: acc)
    in
    let annos = go [] in
    annos @ parse_anno_groups p
  | Lexer.TLAnnoStart ->
    ignore (advance p);
    let rec go acc =
      match (peek p).tok with
      | Lexer.TLAnnoEnd ->
        ignore (advance p);
        List.rev acc
      | Lexer.TEOF -> List.rev acc
      | _ -> go (parse_anno p :: acc)
    in
    let annos = go [] in
    (* a //@ annotation must not be stretched over multiple lines using
       multi-line comments *)
    (match List.rev annos with
    | last :: _ when last.a_loc.end_p.line <> t.span.start_p.line ->
      err
        (Loc.span_of t.span last.a_loc)
        "Single-line annotations cannot be extended to multiple lines with /* multiline comments */ like this"
    | _ -> ());
    annos @ parse_anno_groups p
  | _ -> []

let assert_of_anno (p : t) ~(context : string) (a : anno) : stmt =
  ignore p;
  if a.a_kind <> AAssert then
    err a.a_loc "%s, %s is not permitted" context (anno_name a.a_kind);
  { s = Assert { contract = true; test = a.a_test }; sloc = a.a_loc }

let invariants_of_annos (p : t) (annos : anno list) : expr list =
  List.map
    (fun a ->
      ignore p;
      if a.a_kind <> ALoopInvariant then
        err a.a_loc "The only annotations allowed are loop invariants, %s is not permitted"
          (anno_name a.a_kind);
      a.a_test)
    annos

let contracts_of_annos (annos : anno list) : expr list * expr list =
  let pre = ref [] and post = ref [] in
  List.iter
    (fun a ->
      match a.a_kind with
      | ARequires -> pre := a.a_test :: !pre
      | AEnsures -> post := a.a_test :: !post
      | _ ->
        err a.a_loc "The only annotations allowed are requires and ensures, %s is not permitted"
          (anno_name a.a_kind))
    annos;
  (List.rev !pre, List.rev !post)

(* ------------------------------------------------------------------ *)
(* Statements                                                          *)
(* ------------------------------------------------------------------ *)

(* Convert a parsed statement-expression into a simple statement. *)
let simple_of_sexp (p : t) (se : sexp) (span : Loc.span) : stmt =
  match se with
  | SAssign (op, l, r, _) ->
    (match op with
    | AEq | ATimes | ADiv | AMod | APlus | AMinus -> ()
    | _ -> atleast p span Lang.L2 (Printf.sprintf "assignment operator '%s'" (asnop_string op)));
    let lhs = to_lvalue p l in
    let rhs = to_expr r in
    { s = Assign { op; lhs; rhs }; sloc = span }
  | SUpdate (op, arg, _) ->
    atleast p span Lang.L2
      (Printf.sprintf "postfix update 'x%s'" (match op with `Incr -> "++" | `Decr -> "--"));
    { s = Update { op; arg = to_lvalue p arg }; sloc = span }
  | SAssertE (test, _) ->
    atleast p span Lang.L3 "'assert()'";
    { s = Assert { contract = false; test }; sloc = span }
  | SErrorE (arg, _) ->
    atleast p span Lang.C0 "'error()'";
    { s = ErrorStmt arg; sloc = span }
  | Ex e -> { s = ExprStmt e; sloc = span }

(* Parse a "simple" statement body (no trailing semicolon): either a variable
   declaration or an expression-ish statement. *)
let parse_simple (p : t) : [ `Decl of typ * ident * expr option * Loc.span | `Sexp of sexp ] =
  if starts_type p then begin
    let kind = parse_value_type p in
    let id = var_ident p in
    let init =
      if is_sym p "=" then begin
        ignore (advance p);
        Some (to_expr (parse_expr p))
      end
      else None
    in
    let start_span = match kind.tloc with Some s -> s | None -> (peek p).span in
    let end_span =
      match init with
      | Some e -> e.eloc
      | None -> ( match id.iloc with Some s -> s | None -> start_span)
    in
    `Decl (kind, id, init, Loc.span_of start_span end_span)
  end
  else `Sexp (parse_expr p)

let rec parse_stmt (p : t) : stmt =
  let t = peek p in
  match t.tok with
  | Lexer.TSym "{" -> parse_block p
  | Lexer.TKw "if" ->
    ignore (advance p);
    atleast p t.span Lang.L2 "'if' and 'else'";
    ignore (expect_sym p "(");
    let test = to_expr (parse_expr p) in
    ignore (expect_sym p ")");
    let cons = parse_branch p in
    if is_kw p "else" then begin
      ignore (advance p);
      let alt = parse_branch p in
      { s = If { test; cons; alt = Some alt }; sloc = Loc.span_of t.span alt.sloc }
    end
    else { s = If { test; cons; alt = None }; sloc = Loc.span_of t.span cons.sloc }
  | Lexer.TKw "while" ->
    ignore (advance p);
    atleast p t.span Lang.L2 "'while' loops";
    ignore (expect_sym p "(");
    let test = to_expr (parse_expr p) in
    ignore (expect_sym p ")");
    let annos = parse_anno_groups p in
    let invariants = invariants_of_annos p annos in
    let body = parse_stmt p in
    { s = While { invariants; test; body }; sloc = Loc.span_of t.span body.sloc }
  | Lexer.TKw "for" ->
    ignore (advance p);
    atleast p t.span Lang.L2 "'for' loops";
    ignore (expect_sym p "(");
    let init =
      if is_sym p ";" then None
      else
        Some
          (match parse_simple p with
          | `Decl (kind, id, init, span) -> { s = VarDecl { kind; id; init }; sloc = span }
          | `Sexp se -> simple_of_sexp p se (sexp_loc se))
    in
    ignore (expect_sym p ";");
    let test = to_expr (parse_expr p) in
    ignore (expect_sym p ";");
    let update =
      if is_sym p ")" then None
      else begin
        let se = parse_expr p in
        Some (simple_of_sexp p se (sexp_loc se))
      end
    in
    ignore (expect_sym p ")");
    let annos = parse_anno_groups p in
    let invariants = invariants_of_annos p annos in
    let body = parse_stmt p in
    { s = For { invariants; init; test; update; body }; sloc = Loc.span_of t.span body.sloc }
  | Lexer.TKw "return" ->
    ignore (advance p);
    let arg = if is_sym p ";" then None else Some (to_expr (parse_expr p)) in
    let semi = expect_sym p ";" in
    { s = Return arg; sloc = Loc.span_of t.span semi.span }
  | Lexer.TKw "break" ->
    ignore (advance p);
    atleast p t.span Lang.C1 "'break'";
    let semi = expect_sym p ";" in
    { s = Break; sloc = Loc.span_of t.span semi.span }
  | Lexer.TKw "continue" ->
    ignore (advance p);
    atleast p t.span Lang.C1 "'continue'";
    let semi = expect_sym p ";" in
    { s = Continue; sloc = Loc.span_of t.span semi.span }
  | _ -> (
    match parse_simple p with
    | `Decl (kind, id, init, span) ->
      let semi = expect_sym p ";" in
      { s = VarDecl { kind; id; init }; sloc = Loc.span_of span semi.span }
    | `Sexp se ->
      let semi = expect_sym p ";" in
      simple_of_sexp p se (Loc.span_of (sexp_loc se) semi.span))

(* The branch of an if/else: annotations directly on a branch must be asserts
   and get wrapped in a block together with the statement. *)
and parse_branch (p : t) : stmt =
  let annos = parse_anno_groups p in
  let stm = parse_stmt p in
  match annos with
  | [] -> stm
  | first :: _ ->
    let asserts =
      List.map
        (assert_of_anno p ~context:"The only annotations allowed with if-statements are assertions")
        annos
    in
    let span = Loc.span_of first.a_loc stm.sloc in
    { s = Block { body = asserts @ [ stm ]; block_env = None }; sloc = span }

and parse_block (p : t) : stmt =
  let open_tok = expect_sym p "{" in
  let rec go acc =
    match (peek p).tok with
    | Lexer.TSym "}" ->
      let close = advance p in
      (List.rev acc, close)
    | Lexer.TEOF -> err (peek p).span "expected '}' to close the block"
    | _ -> (
      (* recover per statement so one error doesn't derail the whole block *)
      match
        try
          let annos = parse_anno_groups p in
          let asserts =
            List.map (assert_of_anno p ~context:"Only assert annotations are allowed here") annos
          in
          let acc = List.rev_append asserts acc in
          match (peek p).tok with
          | Lexer.TSym "}" | Lexer.TEOF -> `Stop acc
          | _ -> `Cont (parse_stmt p :: acc)
        with Err.Parse_error (span, msg) ->
          p.diags <- { Err.d_loc = Some span; d_msg = msg; d_severity = Err.Error } :: p.diags;
          recover_stmt p;
          `Cont acc
      with
      | `Cont acc -> go acc
      | `Stop acc -> go acc)
  in
  let body, close = go [] in
  { s = Block { body; block_env = None }; sloc = Loc.span_of open_tok.span close.span }

(* Skip to the next ';' (consumed) or the enclosing '}' (left in place). *)
and recover_stmt (p : t) : unit =
  let depth = ref 0 in
  let stop = ref false in
  while not !stop do
    match (peek p).tok with
    | Lexer.TEOF -> stop := true
    | Lexer.TSym "}" when !depth = 0 -> stop := true
    | Lexer.TSym "}" ->
      decr depth;
      ignore (advance p)
    | Lexer.TSym "{" ->
      incr depth;
      ignore (advance p)
    | Lexer.TSym ";" when !depth = 0 ->
      ignore (advance p);
      stop := true
    | _ -> ignore (advance p)
  done

(* ------------------------------------------------------------------ *)
(* Declarations                                                        *)
(* ------------------------------------------------------------------ *)

let parse_params (p : t) : param list =
  ignore (expect_sym p "(");
  if is_sym p ")" then begin
    ignore (advance p);
    []
  end
  else begin
    let one () =
      let kind = parse_value_type p in
      let id = var_ident p in
      let start_span = match kind.tloc with Some s -> s | None -> (peek p).span in
      let end_span = match id.iloc with Some s -> s | None -> start_span in
      { p_kind = kind; p_id = id; p_loc = Some (Loc.span_of start_span end_span) }
    in
    let rec go acc =
      let param = one () in
      if is_sym p "," then begin
        ignore (advance p);
        go (param :: acc)
      end
      else begin
        ignore (expect_sym p ")");
        List.rev (param :: acc)
      end
    in
    go []
  end

let parse_fundecl (p : t) ~(returns : typ) ~(start : Loc.span) (doc : string) : decl =
  let id = var_ident p in
  let params = parse_params p in
  let annos = parse_anno_groups p in
  let preconds, postconds = contracts_of_annos annos in
  let t = peek p in
  match t.tok with
  | Lexer.TSym ";" ->
    ignore (advance p);
    atleast p start Lang.L3 "function declarations";
    if id.name <> "main" then atleast p start Lang.L3 "functions aside from 'main'";
    FunDecl
      {
        returns; fname = id; params; preconds; postconds;
        fbody = None; fdoc = doc; floc = Some (Loc.span_of start t.span);
        is_local_to = None;
      }
  | Lexer.TSym "{" ->
    if id.name <> "main" then atleast p start Lang.L3 "functions aside from 'main'";
    let body = parse_block p in
    FunDecl
      {
        returns; fname = id; params; preconds; postconds;
        fbody = Some body; fdoc = doc; floc = Some (Loc.span_of start body.sloc);
        is_local_to = None;
      }
  | d -> err t.span "expected ';' or a function body but found %s" (Lexer.tok_string d)

let parse_struct_decl (p : t) (doc : string) : decl =
  (* 'struct' consumed by caller? No: consume here *)
  let struct_tok = expect_kw p "struct" in
  atleast p struct_tok.span Lang.L4 "structs";
  let id = any_ident p in
  let t = peek p in
  match t.tok with
  | Lexer.TSym ";" ->
    ignore (advance p);
    StructDecl
      { s_id = id; s_fields = None; s_doc = doc; s_loc = Some (Loc.span_of struct_tok.span t.span) }
  | Lexer.TSym "{" ->
    ignore (advance p);
    let rec go acc =
      match (peek p).tok with
      | Lexer.TSym "}" ->
        ignore (advance p);
        List.rev acc
      | Lexer.TEOF -> err (peek p).span "expected '}' to close the struct definition"
      | _ ->
        let kind = parse_value_type p in
        let fid = any_ident p in
        ignore (expect_sym p ";");
        let start_span = match kind.tloc with Some s -> s | None -> t.span in
        let end_span = match fid.iloc with Some s -> s | None -> start_span in
        go ({ p_kind = kind; p_id = fid; p_loc = Some (Loc.span_of start_span end_span) } :: acc)
    in
    let fields = go [] in
    let semi = expect_sym p ";" in
    StructDecl
      {
        s_id = id;
        s_fields = Some fields;
        s_doc = doc;
        s_loc = Some (Loc.span_of struct_tok.span semi.span);
      }
  | _ ->
    (* 'struct foo' is the start of a type in a function declaration *)
    let end_span = match id.iloc with Some s -> s | None -> struct_tok.span in
    let base = { t = StructT id; tloc = Some (Loc.span_of struct_tok.span end_span) } in
    let returns = parse_type_postfix p base in
    parse_fundecl p ~returns ~start:struct_tok.span doc

let parse_typedef (p : t) (doc : string) : decl =
  let td_tok = expect_kw p "typedef" in
  atleast p td_tok.span Lang.L3 "typedefs";
  let kind = parse_type p in
  let id = var_ident p in
  (* Register right away so following code can use the new type name. *)
  add_type_id p id.name;
  let id_span = match id.iloc with Some s -> s | None -> td_tok.span in
  if is_sym p "(" then begin
    (* function type definition: typedef ret name(args) [contracts]; *)
    atleast p td_tok.span Lang.C1 "function types";
    let params = parse_params p in
    let annos = parse_anno_groups p in
    let preconds, postconds = contracts_of_annos annos in
    let semi =
      if is_sym p ";" then advance p
      else err (peek p).span "typedef is missing its trailing semicolon"
    in
    FunTypeDef
      {
        returns = kind; fname = id; params; preconds; postconds;
        fbody = None; fdoc = doc; floc = Some (Loc.span_of td_tok.span semi.span);
        is_local_to = None;
      }
  end
  else begin
    let kind = check_value_type p kind in
    let semi =
      if is_sym p ";" then advance p
      else err (peek p).span "typedef is missing its trailing semicolon"
    in
    let start_span = match kind.tloc with Some s -> s | None -> td_tok.span in
    TypeDef
      {
        td_def = { p_kind = kind; p_id = id; p_loc = Some (Loc.span_of start_span id_span) };
        td_doc = doc;
        td_loc = Some (Loc.span_of td_tok.span semi.span);
      }
  end

(* Parse a '#use' pragma line. Returns None for unknown pragmas. *)
let parse_use_pragma (text : string) : [ `Lib of string | `File of string ] option =
  let n = String.length text in
  let is_space c = c = ' ' || c = '\t' in
  let skip_ws i =
    let i = ref i in
    while !i < n && is_space text.[!i] do incr i done;
    !i
  in
  if n >= 4 && String.sub text 0 4 = "#use" then begin
    let i = skip_ws 4 in
    if i >= n then None
    else if text.[i] = '<' then begin
      match String.index_from_opt text i '>' with
      | Some j when j > i + 1 ->
        let name = String.sub text (i + 1) (j - i - 1) in
        let rest = skip_ws (j + 1) in
        let ok_name =
          String.for_all (fun c -> Lexer.is_ident_char c) name && String.length name > 0
        in
        if rest = n && ok_name then Some (`Lib name) else None
      | _ -> None
    end
    else if text.[i] = '"' then begin
      match String.index_from_opt text (i + 1) '"' with
      | Some j when j > i + 1 ->
        let path = String.sub text (i + 1) (j - i - 1) in
        let rest = skip_ws (j + 1) in
        if rest = n then Some (`File path) else None
      | _ -> None
    end
    else None
  end
  else None

(* Skip tokens until a plausible declaration boundary: a ';' or closing '}'
   at brace depth 0, or something that looks like the start of a new
   top-level declaration at the beginning of a line. *)
let recover (p : t) : unit =
  let depth = ref 0 in
  let stop = ref false in
  (* always make progress, even if the offending token looks like a
     declaration start *)
  (match (peek p).tok with
  | Lexer.TEOF -> stop := true
  | Lexer.TSym "{" ->
    depth := 1;
    ignore (advance p)
  | Lexer.TSym (";" | "}") -> ignore (advance p)
  | _ -> ignore (advance p));
  while not !stop do
    let t = peek p in
    match t.tok with
    | Lexer.TEOF -> stop := true
    | (Lexer.TKw ("int" | "bool" | "string" | "char" | "void" | "struct" | "typedef") | Lexer.TPragma _)
      when !depth = 0 && t.span.start_p.col = 1 ->
      stop := true
    | Lexer.TSym "{" ->
      incr depth;
      ignore (advance p)
    | Lexer.TSym "}" ->
      ignore (advance p);
      if !depth <= 1 then begin
        (* consume a trailing ';' (as after a struct definition) *)
        if is_sym p ";" then ignore (advance p);
        stop := true
      end
      else decr depth
    | Lexer.TSym ";" when !depth = 0 ->
      ignore (advance p);
      stop := true
    | _ -> ignore (advance p)
  done

let parse_decl (p : t) : decl option =
  let t = peek p in
  match t.tok with
  | Lexer.TPragma text -> (
    ignore (advance p);
    clear_doc p;
    match parse_use_pragma text with
    | Some (`Lib lib) -> Some (UseLib { lib; ul_loc = Some t.span })
    | Some (`File path) -> Some (UseFile { path; uf_loc = Some t.span })
    | None -> None (* unknown pragmas are accepted and ignored *))
  | Lexer.TKw "struct" ->
    let doc = take_doc p in
    Some (parse_struct_decl p doc)
  | Lexer.TKw "typedef" ->
    let doc = take_doc p in
    Some (parse_typedef p doc)
  | _ when starts_type p ->
    let doc = take_doc p in
    let returns = parse_type p in
    let start = match returns.tloc with Some s -> s | None -> t.span in
    Some (parse_fundecl p ~returns ~start doc)
  | Lexer.TAnnoStart | Lexer.TLAnnoStart ->
    err t.span "annotations must be attached to a function declaration or appear inside a function body"
  | d -> err t.span "expected a declaration but found %s" (Lexer.tok_string d)

type result = {
  decls : decl list;
  diagnostics : Err.diagnostic list;
  typedefs : string list;
}

let parse_program (p : t) : result =
  let decls = ref [] in
  let stop = ref false in
  while not !stop do
    p.capture <- true;
    let t = peek p in
    p.capture <- false;
    match t.tok with
    | Lexer.TEOF -> stop := true
    | _ -> (
      try match parse_decl p with Some d -> decls := d :: !decls | None -> ()
      with Err.Parse_error (span, msg) ->
        p.diags <- { Err.d_loc = Some span; d_msg = msg; d_severity = Err.Error } :: p.diags;
        recover p)
  done;
  let lex_errors =
    List.rev_map
      (fun (span, msg) -> { Err.d_loc = Some span; d_msg = msg; d_severity = Err.Error })
      p.lx.Lexer.errors
  in
  { decls = List.rev !decls; diagnostics = List.rev p.diags @ lex_errors; typedefs = type_ids p }

(* Parse a single string as an expression (used for completion contexts). *)
let expression_of_string ?(typeids = []) (src : string) : expr option =
  let p = create ~lang:Lang.C1 ~typeids src in
  try
    let se = parse_expr p in
    match (peek p).tok with
    | Lexer.TEOF -> ( match se with Ex e -> Some e | _ -> None)
    | _ -> None
  with Err.Parse_error _ -> None
