(* Errors raised by the parser and typechecker. Both carry an optional source
   span; typing errors additionally carry a severity and optional hints
   (rendered as "\n\nHint: ..." like the reference implementation). *)

type severity = Error | Warning | Information | Hint

exception Parse_error of Loc.span * string

let parse_error span fmt = Printf.ksprintf (fun m -> raise (Parse_error (span, m))) fmt

type terror = {
  msg : string; (* already includes hints *)
  tloc : Loc.span option;
  severity : severity;
}

exception Type_error of terror

let with_hints (msg : string) (hints : string list) : string =
  match hints with
  | [] -> msg
  | _ -> msg ^ "\n\nHint: " ^ String.concat "\n      " hints

let type_error ?(severity = Error) ?loc ?(hints = []) msg =
  raise (Type_error { msg = with_hints msg hints; tloc = loc; severity })

(* A diagnostic ready to be shown to the user. *)
type diagnostic = {
  d_loc : Loc.span option;
  d_msg : string;
  d_severity : severity;
}

let severity_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Information -> "info"
  | Hint -> "hint"
