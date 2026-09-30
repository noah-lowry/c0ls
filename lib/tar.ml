(* Minimal tar (ustar) reader, enough to list the sources inside the .o0/.o1
   "object files" produced by newcc0 (which are tar archives of C0 sources).
   Gzipped archives are decompressed by shelling out to gzip when present. *)

let read_file (path : string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let is_gzip (data : string) : bool =
  String.length data >= 2 && data.[0] = '\x1f' && data.[1] = '\x8b'

let gunzip (path : string) : string option =
  try
    let ic = Unix.open_process_in (Printf.sprintf "gzip -dc %s 2>/dev/null" (Filename.quote path)) in
    let buf = Buffer.create 65536 in
    let chunk = Bytes.create 65536 in
    let rec loop () =
      let n = input ic chunk 0 (Bytes.length chunk) in
      if n > 0 then begin
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
      end
    in
    (try loop () with End_of_file -> ());
    (match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Some (Buffer.contents buf)
    | _ -> None)
  with _ -> None

let trim_nul (s : string) : string =
  match String.index_opt s '\000' with Some i -> String.sub s 0 i | None -> s

let parse_octal (s : string) : int option =
  let s = String.trim (trim_nul s) in
  if s = "" then Some 0
  else
    try Some (int_of_string ("0o" ^ s)) with _ -> None

(* Returns [(name, contents); ...] for regular files in the archive. *)
let entries_of_data (data : string) : (string * string) list =
  let len = String.length data in
  let result = ref [] in
  let pos = ref 0 in
  let stop = ref false in
  while (not !stop) && !pos + 512 <= len do
    let header = String.sub data !pos 512 in
    if String.for_all (fun c -> c = '\000') header then stop := true
    else begin
      let name = trim_nul (String.sub header 0 100) in
      let size = Option.value (parse_octal (String.sub header 124 12)) ~default:0 in
      let typeflag = header.[156] in
      let prefix = if String.length header >= 500 then trim_nul (String.sub header 345 155) else "" in
      let full_name = if prefix = "" then name else prefix ^ "/" ^ name in
      let data_start = !pos + 512 in
      if data_start + size <= len && (typeflag = '0' || typeflag = '\000') && full_name <> "" then
        result := (full_name, String.sub data data_start size) :: !result;
      let blocks = (size + 511) / 512 in
      pos := data_start + (blocks * 512)
    end
  done;
  List.rev !result

let read_archive (path : string) : (string * string) list option =
  try
    let data = read_file path in
    let data = if is_gzip data then match gunzip path with Some d -> d | None -> data else data in
    Some (entries_of_data data)
  with _ -> None
