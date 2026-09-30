(* Small helpers shared across modules. *)

(* Minimum edit distance (insertions/deletions only, like the reference). *)
let edit_distance (s : string) (t : string) : int =
  let n = String.length s and m = String.length t in
  let memo = Array.make_matrix (n + 1) (m + 1) (-1) in
  let rec loop i j =
    if memo.(i).(j) >= 0 then memo.(i).(j)
    else begin
      let result =
        if i = 0 then j
        else if j = 0 then i
        else if s.[i - 1] = t.[j - 1] then loop (i - 1) (j - 1)
        else 1 + min (loop i (j - 1)) (loop (i - 1) j)
      in
      memo.(i).(j) <- result;
      result
    end
  in
  loop n m

(* Up to [num] closest candidates to [target]. *)
let best_matches ?(num = 3) (target : string) (candidates : string list) : string list =
  let uniq = List.sort_uniq compare candidates in
  uniq
  |> List.map (fun str -> (edit_distance target str, str))
  |> List.sort compare
  |> List.filteri (fun i _ -> i < num)
  |> List.map snd

(* How a file is shown to the user in messages ("foo.c0" or a
   workspace-relative path). The server installs a smarter version. *)
let display_path : (string -> string) ref =
  ref (fun uri ->
      let path =
        if String.length uri >= 7 && String.sub uri 0 7 = "file://" then
          String.sub uri 7 (String.length uri - 7)
        else uri
      in
      Filename.basename path)

let starts_with ~(prefix : string) (s : string) : bool =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let ends_with ~(suffix : string) (s : string) : bool =
  let ls = String.length s and lx = String.length suffix in
  ls >= lx && String.sub s (ls - lx) lx = suffix
