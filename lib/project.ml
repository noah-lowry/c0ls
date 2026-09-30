(* Project support:
   - locating the bundled C0 library headers (c0lib/<name>.h0)
   - discovering the dependencies of a file from a README.txt / project.txt
     containing a "% cc0 ..." command line (the 15-122 convention): every C0
     file listed before the current one is a dependency and is parsed first. *)

let file_exists_dir (d : string) : bool = Sys.file_exists d && Sys.is_directory d

(* Directory containing the C0 library headers. Overridable with C0LS_LIB_DIR. *)
let lib_dir : unit -> string option =
  let cached = ref None in
  fun () ->
    match !cached with
    | Some result -> result
    | None ->
      let exe_dir = Filename.dirname Sys.executable_name in
      let candidates =
        (match Sys.getenv_opt "C0LS_LIB_DIR" with Some d -> [ d ] | None -> [])
        @ [
            (* installed layout: <prefix>/bin/c0ls, <prefix>/share/c0ls/c0lib *)
            Filename.concat exe_dir "../share/c0ls/c0lib";
            Filename.concat exe_dir "c0lib";
            (* development layout: _build/default/bin/main.exe *)
            Filename.concat exe_dir "../../../c0lib";
          ]
      in
      let result = List.find_opt file_exists_dir candidates in
      cached := Some result;
      result

let lib_header_path (libname : string) : string option =
  match lib_dir () with
  | None -> None
  | Some dir ->
    let path = Filename.concat dir (libname ^ ".h0") in
    if Sys.file_exists path then Some path else None

(* --------------------------------------------------------------- *)
(* Globbing: supports the wildcards "star" and "?"                  *)
(* --------------------------------------------------------------- *)

let glob_has_magic (s : string) : bool = String.exists (fun c -> c = '*' || c = '?' || c = '[') s

let pattern_matches (pattern : string) (name : string) : bool =
  (* translate the glob to a simple regex-free matcher *)
  let np = String.length pattern and nn = String.length name in
  (* dynamic programming over pattern/name positions *)
  let rec go pi ni =
    if pi = np then ni = nn
    else
      match pattern.[pi] with
      | '*' -> (ni <= nn && go (pi + 1) ni) || (ni < nn && name.[ni] <> '/' && go pi (ni + 1))
      | '?' -> ni < nn && name.[ni] <> '/' && go (pi + 1) (ni + 1)
      | c -> ni < nn && name.[ni] = c && go (pi + 1) (ni + 1)
  in
  go 0 0

(* Expand a glob relative to [cwd]; returns matches sorted, relative paths. *)
let glob (cwd : string) (pattern : string) : string list =
  let dir_part = Filename.dirname pattern in
  let base_pattern = Filename.basename pattern in
  let search_dir = if dir_part = "." && not (Util.starts_with ~prefix:"./" pattern) then cwd else Filename.concat cwd dir_part in
  let prefix = if dir_part = "." && not (Util.starts_with ~prefix:"./" pattern) then "" else dir_part ^ "/" in
  match Sys.readdir search_dir with
  | entries ->
    entries |> Array.to_list
    |> List.filter (fun name -> pattern_matches base_pattern name)
    |> List.sort compare
    |> List.map (fun name -> prefix ^ name)
  | exception Sys_error _ -> []

(* --------------------------------------------------------------- *)
(* README.txt / project.txt dependencies                            *)
(* --------------------------------------------------------------- *)

type dependencies = {
  config_path : string; (* the README.txt/project.txt used *)
  dep_paths : string list; (* absolute paths, in order *)
}

let read_lines (path : string) : string list =
  try String.split_on_char '\n' (Tar.read_file path) with _ -> []

(* Split a command line on whitespace. *)
let split_args (line : string) : string list =
  String.split_on_char ' ' line
  |> List.concat_map (String.split_on_char '\t')
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let is_cc0_line (line : string) : bool =
  (* ^\s*%\s*cc0 *)
  let trimmed = String.trim line in
  if not (Util.starts_with ~prefix:"%" trimmed) then false
  else
    let rest = String.trim (String.sub trimmed 1 (String.length trimmed - 1)) in
    Util.starts_with ~prefix:"cc0" rest

(* Try to find [target_path]'s dependencies in one config file. *)
let deps_from_config (config_path : string) (target_path : string) : dependencies option =
  if not (Sys.file_exists config_path) then None
  else begin
    let cwd = Filename.dirname config_path in
    (* the name the config would use for the target *)
    let target_rel =
      if Filename.is_relative target_path then target_path
      else if Util.starts_with ~prefix:(cwd ^ "/") target_path then
        String.sub target_path (String.length cwd + 1) (String.length target_path - String.length cwd - 1)
      else target_path
    in
    let found = ref None in
    List.iter
      (fun line ->
        if !found = None && is_cc0_line line then begin
          let args = split_args line in
          let deps = ref [] in
          let matched = ref false in
          let skip_next = ref false in
          List.iter
            (fun arg ->
              if !matched then ()
              else if !skip_next then skip_next := false
              else
                match arg with
                | "%" | "cc0" -> ()
                | "-o" -> skip_next := true
                | _ when String.length arg > 0 && arg.[0] = '-' -> ()
                | _ when glob_has_magic arg ->
                  List.iter
                    (fun file ->
                      if not !matched && Lang.is_c0_file file then
                        if file = target_rel then matched := true
                        else deps := Filename.concat cwd file :: !deps)
                    (glob cwd arg)
                | _ ->
                  if arg = target_rel then matched := true
                  else if Lang.is_c0_file arg then deps := Filename.concat cwd arg :: !deps)
            args;
          if !matched then found := Some { config_path; dep_paths = List.rev !deps }
        end)
      (read_lines config_path);
    !found
  end

(* Search the usual places for a config that mentions [target_path]. *)
let find_dependencies ?(workspace_root : string option) (target_path : string) : dependencies option
    =
  let dir = Filename.dirname target_path in
  let candidates =
    [
      Filename.concat dir "README.txt";
      Filename.concat (Filename.dirname dir) "README.txt";
      Filename.concat dir "project.txt";
      Filename.concat (Filename.dirname dir) "project.txt";
    ]
    @ (match workspace_root with
      | Some root -> [ Filename.concat root "project.txt" ]
      | None -> [])
  in
  List.fold_left
    (fun acc config -> match acc with Some _ -> acc | None -> deps_from_config config target_path)
    None candidates
