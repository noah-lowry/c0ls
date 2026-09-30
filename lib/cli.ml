(* `c0ls check file.c0 ...` — run the parser and typechecker from the command
   line and print diagnostics in the usual file:line:col format. *)

let print_diagnostic (display : string) (d : Err.diagnostic) : unit =
  let line, col =
    match d.Err.d_loc with
    | Some sp -> (sp.Loc.start_p.line, sp.Loc.start_p.col)
    | None -> (1, 1)
  in
  (* indent continuation lines of multi-line messages *)
  let message =
    match String.split_on_char '\n' d.Err.d_msg with
    | [] -> ""
    | first :: rest ->
      String.concat "\n"
        (first :: List.map (fun l -> if String.trim l = "" then "" else "    " ^ l) rest)
  in
  Printf.printf "%s:%d:%d: %s: %s\n" display line col
    (Err.severity_string d.Err.d_severity)
    message

let check_files (files : string list) : int =
  let saw_error = ref false in
  List.iter
    (fun file ->
      if not (Sys.file_exists file) then begin
        Printf.eprintf "c0ls: %s: no such file\n" file;
        saw_error := true
      end
      else begin
        let contents = Tar.read_file file in
        let uri = Validate.uri_of_path file in
        let result = Validate.check_document ~uri ~contents () in
        List.iter (fun n -> Printf.eprintf "c0ls: note: %s\n" n) result.Validate.notices;
        List.iter (print_diagnostic file) result.Validate.diagnostics;
        if
          List.exists
            (fun (d : Err.diagnostic) -> d.Err.d_severity = Err.Error)
            result.Validate.diagnostics
        then saw_error := true
      end)
    files;
  if !saw_error then 1 else 0
