(* Heuristic completion-context detection (port of c0Completions.ts):
   scanning backwards from the cursor, decide whether the user is typing
   a struct field access (foo->ba|), a function argument (foo(x, |), or a
   contract annotation (//@ requ|). *)

type context =
  | StructAccess of { expr : Ast.expr; dereferenced : bool }
  | FunctionCall of { name : string; argument_number : int }
  | ContractDecl

let scan_expression (source : string) (index : int) : string =
  let paren_stack = ref 0 in
  let brace_stack = ref 0 in
  let pos = ref index in
  let stop = ref false in
  while (not !stop) && !pos >= 0 do
    let c = source.[!pos] in
    (* skip over -> *)
    if !pos >= 1 && source.[!pos - 1] = '-' && c = '>' then pos := !pos - 2
    else begin
      if c = ')' then incr paren_stack;
      if c = '(' then begin
        if !paren_stack = 0 then stop := true else decr paren_stack
      end;
      if not !stop then begin
        if !paren_stack = 0 then begin
          (match c with
          | ';' | ',' | '{' -> stop := true
          | _ when String.contains "!~-*+/%><&^|?:=" c -> stop := true
          | _ -> ());
          ()
        end;
        if c = '\n' then stop := true;
        if c = ']' then incr brace_stack;
        if c = '[' then begin
          if !brace_stack = 0 then stop := true else decr brace_stack
        end;
        if !pos >= 6 && String.sub source (!pos - 6) 6 = "return" then stop := true;
        if not !stop then decr pos
      end
    end
  done;
  if !pos < 0 then String.trim (String.sub source 0 (index + 1))
  else String.trim (String.sub source (!pos + 1) (index - !pos))

let scan_function_name (source : string) (index : int) : string =
  let pos = ref index in
  while
    !pos >= 0
    &&
    let c = source.[!pos] in
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_'
  do
    decr pos
  done;
  String.trim (String.sub source (!pos + 1) (index - !pos))

let get_context ?(typeids : string list = []) (source : string) (index : int) : context option =
  let n = String.length source in
  if n = 0 then None
  else begin
    let index = min index (n - 1) in
    let pos = ref index in
    while !pos > 0 && source.[!pos] = ' ' do decr pos done;
    let starts_at offset s =
      let l = String.length s in
      !pos - offset >= 0 && !pos - offset + l <= n && String.sub source (!pos - offset) l = s
    in
    if starts_at 3 "//@" || starts_at 3 "/*@" then Some ContractDecl
    else begin
      let struct_access =
        if starts_at 2 "->" then Some true
        else if !pos >= 1 && source.[!pos - 1] = '.' then Some false
        else None
      in
      match struct_access with
      | Some dereferenced -> (
        let scan_from = !pos - if dereferenced then 3 else 2 in
        if scan_from < 0 then None
        else
          let text = scan_expression source scan_from in
          match Parser.expression_of_string ~typeids text with
          | Some expr -> Some (StructAccess { expr; dereferenced })
          | None -> None)
      | None ->
        (* look for an enclosing function call *)
        let paren_stack = ref 0 in
        let argument_number = ref 0 in
        let result = ref None in
        let p = ref index in
        let stop = ref false in
        while (not !stop) && !p >= 0 do
          (match source.[!p] with
          | ';' -> stop := true
          | ',' when !paren_stack = 0 -> incr argument_number
          | ')' -> incr paren_stack
          | '(' ->
            if !paren_stack = 0 then begin
              let name = if !p = 0 then "" else scan_function_name source (!p - 1) in
              if name <> "" then
                result := Some (FunctionCall { name; argument_number = !argument_number });
              stop := true
            end
            else decr paren_stack
          | _ -> ());
          decr p
        done;
        !result
    end
  end
