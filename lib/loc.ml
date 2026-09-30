(* Source positions are 1-indexed (line and column), matching the reference
   implementation. LSP positions are 0-indexed; conversion happens at the
   server boundary. *)

type pos = { line : int; col : int }

type span = {
  start_p : pos;
  end_p : pos;
  (* The URI of the file this span belongs to. Mutable because sources are
     stamped onto declarations after a file is parsed. *)
  mutable source : string option;
}

let mk start_p end_p = { start_p; end_p; source = None }

let span_of (a : span) (b : span) = { start_p = a.start_p; end_p = b.end_p; source = None }

type ord = Less | Equal | Greater

let compare_pos (a : pos) (b : pos) : ord =
  if a.line < b.line then Less
  else if a.line > b.line then Greater
  else if a.col < b.col then Less
  else if a.col > b.col then Greater
  else Equal

let is_inside (p : pos) (sp : span option) : bool =
  match sp with
  | None -> false
  | Some sp ->
    compare_pos p sp.start_p <> Less && compare_pos p sp.end_p <> Greater

let to_string (sp : span) =
  Printf.sprintf "%d.%d-%d.%d" sp.start_p.line sp.start_p.col sp.end_p.line sp.end_p.col
