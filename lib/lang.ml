(* Language levels accepted by the CC0 compiler. L1-L4 are the 15-411 lab
   languages; C0 is the 15-122 language; C1 extends C0 with function
   pointers, casts, void*, and break/continue. *)

type t = L1 | L2 | L3 | L4 | C0 | C1

let to_string = function
  | L1 -> "L1"
  | L2 -> "L2"
  | L3 -> "L3"
  | L4 -> "L4"
  | C0 -> "C0"
  | C1 -> "C1"

(* Accepts ".l1", "L1", "c0", ".h0", ... *)
let parse (s : string) : t option =
  let s = if String.length s > 0 && s.[0] = '.' then String.sub s 1 (String.length s - 1) else s in
  match String.lowercase_ascii s with
  | "l1" -> Some L1
  | "l2" -> Some L2
  | "l3" -> Some L3
  | "l4" -> Some L4
  | "c0" | "h0" -> Some C0
  | "c1" | "h1" -> Some C1
  | _ -> None

let of_filename (name : string) : t option = parse (Filename.extension name)

let is_object_file (name : string) : bool =
  match Filename.extension name with ".o0" | ".o1" -> true | _ -> false

let is_c0_file (name : string) : bool =
  of_filename name <> None || is_object_file name

(* Does [lang] include all features of [needed]? *)
let includes (lang : t) (needed : t) : bool =
  let rank = function L1 -> 1 | L2 -> 2 | L3 -> 3 | L4 -> 4 | C0 -> 5 | C1 -> 6 in
  rank lang >= rank needed
