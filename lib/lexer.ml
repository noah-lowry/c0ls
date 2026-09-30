(* Hand-written lexer for C0/C1.

   Mirrors the staged "moo" lexer of the reference implementation:
   - contract annotations (//@ ... \n and /*@ ... @*/) produce explicit
     open/close tokens, and '@' inside an annotation acts as whitespace;
   - comments nest and are emitted as tokens so the parser can capture
     doc comments preceding top-level declarations;
   - identifiers are NOT classified as typedef names here; the parser
     consults its typedef set when it needs to distinguish them.

   The lexer never raises: malformed input is recorded in [errors] and
   lexing continues, which keeps the parser's error recovery simple. *)

type tok_d =
  | TIdent of string
  | TKw of string
  | TNum of string
  | TStr of string list (* pieces: plain runs or 2-char escape sequences *)
  | TChr of string (* raw contents between the quotes *)
  | TSym of string
  | TBackslash
  | TAnnoStart (* /*@ *)
  | TAnnoEnd (* @*/ *)
  | TLAnnoStart (* //@ *)
  | TLAnnoEnd (* newline (or EOF) terminating a line annotation *)
  | TPragma of string (* full '#...' line *)
  | TComment of { text : string; block : bool }
  | TEOF

type token = { tok : tok_d; span : Loc.span }

type mode = Normal | LineAnno | MultiAnno

type t = {
  src : string;
  annos : bool; (* false for L1-L4: treat //@ and /*@ as plain comments *)
  mutable pos : int;
  mutable line : int;
  mutable bol : int; (* offset of beginning of current line *)
  mutable mode : mode;
  mutable errors : (Loc.span * string) list;
}

let keywords =
  [ "int"; "bool"; "string"; "char"; "void"; "struct"; "typedef"; "if"; "else";
    "while"; "for"; "continue"; "break"; "return"; "assert"; "error"; "true";
    "false"; "NULL"; "alloc"; "alloc_array" ]

let is_keyword s = List.mem s keywords

let create ?(annos = true) (src : string) : t =
  { src; annos; pos = 0; line = 1; bol = 0; mode = Normal; errors = [] }

let cur_pos (lx : t) : Loc.pos = { line = lx.line; col = lx.pos - lx.bol + 1 }

let peek_char (lx : t) : char option =
  if lx.pos < String.length lx.src then Some lx.src.[lx.pos] else None

let peek_at (lx : t) (k : int) : char option =
  if lx.pos + k < String.length lx.src then Some lx.src.[lx.pos + k] else None

let looking_at (lx : t) (s : string) : bool =
  let n = String.length s in
  lx.pos + n <= String.length lx.src && String.sub lx.src lx.pos n = s

(* Advance one character, maintaining line/column bookkeeping. *)
let advance (lx : t) : unit =
  (match peek_char lx with
  | Some '\n' ->
    lx.line <- lx.line + 1;
    lx.bol <- lx.pos + 1
  | _ -> ());
  lx.pos <- lx.pos + 1

let advance_n (lx : t) (n : int) : unit =
  for _ = 1 to n do
    advance lx
  done

let error (lx : t) (span : Loc.span) (msg : string) : unit =
  lx.errors <- (span, msg) :: lx.errors

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'

(* Multi-character operators, longest first. *)
let syms3 = [ "<<="; ">>=" ]
let syms2 =
  [ "<<"; ">>"; "<="; ">="; "=="; "!="; "&&"; "||"; "->"; "++"; "--";
    "+="; "-="; "*="; "/="; "%="; "&="; "^="; "|=" ]
let syms1 = "!$%&()*+,-./:;<=>?[]^{|}~"

let mk_tok (lx : t) (start_p : Loc.pos) (tok : tok_d) : token =
  { tok; span = Loc.mk start_p (cur_pos lx) }

(* Consume a nested block comment; the leading "/*" has been consumed.
   Returns the comment text (between the outermost delimiters). *)
let read_block_comment (lx : t) (start_p : Loc.pos) : string =
  let buf = Buffer.create 32 in
  let depth = ref 1 in
  let rec go () =
    if lx.pos >= String.length lx.src then
      error lx (Loc.mk start_p (cur_pos lx)) "unterminated comment: expected '*/'"
    else if looking_at lx "*/" then begin
      decr depth;
      advance_n lx 2;
      if !depth > 0 then begin
        Buffer.add_string buf "*/";
        go ()
      end
    end
    else if looking_at lx "/*" then begin
      incr depth;
      Buffer.add_string buf "/*";
      advance_n lx 2;
      go ()
    end
    else begin
      Buffer.add_char buf lx.src.[lx.pos];
      advance lx;
      go ()
    end
  in
  go ();
  Buffer.contents buf

(* Consume up to (but not including) the next newline. *)
let read_to_eol (lx : t) : string =
  let start = lx.pos in
  while lx.pos < String.length lx.src && lx.src.[lx.pos] <> '\n' do
    (* keep \r out of the text *)
    advance lx
  done;
  let text = String.sub lx.src start (lx.pos - start) in
  if String.length text > 0 && text.[String.length text - 1] = '\r' then
    String.sub text 0 (String.length text - 1)
  else text

let read_string (lx : t) (start_p : Loc.pos) : token =
  (* opening quote consumed *)
  let pieces = ref [] in
  let buf = Buffer.create 16 in
  let flush_run () =
    if Buffer.length buf > 0 then begin
      pieces := Buffer.contents buf :: !pieces;
      Buffer.clear buf
    end
  in
  let rec go () =
    match peek_char lx with
    | None | Some '\n' ->
      error lx (Loc.mk start_p (cur_pos lx)) "unterminated string literal"
    | Some '"' -> advance lx
    | Some '\\' ->
      (match peek_at lx 1 with
      | None | Some '\n' | Some '\r' ->
        advance lx;
        error lx (Loc.mk start_p (cur_pos lx)) "unterminated string literal"
      | Some c ->
        flush_run ();
        pieces := Printf.sprintf "\\%c" c :: !pieces;
        advance_n lx 2;
        go ())
    | Some c ->
      Buffer.add_char buf c;
      advance lx;
      go ()
  in
  go ();
  flush_run ();
  mk_tok lx start_p (TStr (List.rev !pieces))

let read_char (lx : t) (start_p : Loc.pos) : token =
  (* opening quote consumed *)
  let buf = Buffer.create 4 in
  let rec go () =
    match peek_char lx with
    | None ->
      error lx (Loc.mk start_p (cur_pos lx)) "unterminated character literal"
    | Some '\'' -> advance lx
    | Some '\n' ->
      error lx (Loc.mk start_p (cur_pos lx)) "unterminated character literal"
    | Some '\\' ->
      Buffer.add_char buf '\\';
      advance lx;
      (match peek_char lx with
      | Some c ->
        Buffer.add_char buf c;
        advance lx
      | None -> ());
      go ()
    | Some c ->
      Buffer.add_char buf c;
      advance lx;
      go ()
  in
  go ();
  mk_tok lx start_p (TChr (Buffer.contents buf))

let rec next (lx : t) : token =
  let start_p = cur_pos lx in
  match peek_char lx with
  | None ->
    if lx.mode = LineAnno then begin
      lx.mode <- Normal;
      mk_tok lx start_p TLAnnoEnd
    end
    else mk_tok lx start_p TEOF
  | Some c -> (
    match c with
    | ' ' | '\t' | '\011' | '\012' | '\r' ->
      advance lx;
      next lx
    | '\n' ->
      if lx.mode = LineAnno then begin
        advance lx;
        lx.mode <- Normal;
        mk_tok lx start_p TLAnnoEnd
      end
      else begin
        advance lx;
        next lx
      end
    | _ when lx.mode = MultiAnno && looking_at lx "@*/" ->
      advance_n lx 3;
      lx.mode <- Normal;
      mk_tok lx start_p TAnnoEnd
    | '@' when lx.mode <> Normal ->
      (* '@' acts as whitespace inside annotations *)
      advance lx;
      next lx
    | _ when lx.annos && lx.mode = Normal && looking_at lx "/*@"
             && not (looking_at lx "/*@*/") ->
      advance_n lx 3;
      lx.mode <- MultiAnno;
      mk_tok lx start_p TAnnoStart
    | _ when looking_at lx "/*" ->
      advance_n lx 2;
      let text = read_block_comment lx start_p in
      mk_tok lx start_p (TComment { text; block = true })
    | _ when lx.annos && lx.mode = Normal && looking_at lx "//@" ->
      advance_n lx 3;
      lx.mode <- LineAnno;
      mk_tok lx start_p TLAnnoStart
    | _ when looking_at lx "//" ->
      advance_n lx 2;
      let text = read_to_eol lx in
      (* In a line annotation, a // comment runs to the end of the line; the
         newline (seen next) then closes the annotation. *)
      mk_tok lx start_p (TComment { text; block = false })
    | '#' when lx.mode = Normal ->
      let text = read_to_eol lx in
      mk_tok lx start_p (TPragma text)
    | '"' ->
      advance lx;
      read_string lx start_p
    | '\'' ->
      advance lx;
      read_char lx start_p
    | '\\' ->
      advance lx;
      mk_tok lx start_p TBackslash
    | c when is_ident_start c ->
      let start = lx.pos in
      while (match peek_char lx with Some c -> is_ident_char c | None -> false) do
        advance lx
      done;
      let name = String.sub lx.src start (lx.pos - start) in
      mk_tok lx start_p (if is_keyword name then TKw name else TIdent name)
    | c when is_digit c ->
      (* Overly permissive, like the reference: '123abc' lexes as one numeric
         token and is rejected with a good message during parsing. *)
      let start = lx.pos in
      advance lx;
      while (match peek_char lx with Some c -> is_ident_char c | None -> false) do
        advance lx
      done;
      mk_tok lx start_p (TNum (String.sub lx.src start (lx.pos - start)))
    | _ ->
      let try_sym n =
        if lx.pos + n <= String.length lx.src then Some (String.sub lx.src lx.pos n)
        else None
      in
      let sym3 = match try_sym 3 with Some s when List.mem s syms3 -> Some s | _ -> None in
      let sym2 = match try_sym 2 with Some s when List.mem s syms2 -> Some s | _ -> None in
      (match sym3, sym2 with
      | Some s, _ ->
        advance_n lx 3;
        mk_tok lx start_p (TSym s)
      | None, Some s ->
        advance_n lx 2;
        mk_tok lx start_p (TSym s)
      | None, None ->
        if String.contains syms1 c then begin
          advance lx;
          mk_tok lx start_p (TSym (String.make 1 c))
        end
        else begin
          advance lx;
          error lx
            (Loc.mk start_p (cur_pos lx))
            (Printf.sprintf "invalid character '%s'" (Char.escaped c));
          next lx
        end))

let tok_string (t : tok_d) : string =
  match t with
  | TIdent s -> Printf.sprintf "identifier '%s'" s
  | TKw s -> Printf.sprintf "'%s'" s
  | TNum s -> Printf.sprintf "number '%s'" s
  | TStr _ -> "string literal"
  | TChr _ -> "character literal"
  | TSym s -> Printf.sprintf "'%s'" s
  | TBackslash -> "'\\'"
  | TAnnoStart -> "'/*@'"
  | TAnnoEnd -> "'@*/'"
  | TLAnnoStart -> "'//@'"
  | TLAnnoEnd -> "end of annotation"
  | TPragma _ -> "pragma"
  | TComment _ -> "comment"
  | TEOF -> "end of file"
