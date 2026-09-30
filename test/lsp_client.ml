(* A tiny LSP client used by the cram tests: starts c0ls, opens one file,
   issues a fixed set of requests, and prints compact summaries of the
   responses. Usage:

   lsp_client SERVER FILE hoverLINE hoverCOL complLINE complCOL sigLINE sigCOL

   (lines/columns are 0-based, like the protocol) *)

module J = Yojson.Safe

let member key (j : J.t) : J.t =
  match j with `Assoc fields -> ( try List.assoc key fields with Not_found -> `Null) | _ -> `Null

let to_list = function `List l -> l | _ -> []
let to_string_default d = function `String s -> s | _ -> d

let send (oc : out_channel) (msg : J.t) : unit =
  let data = J.to_string msg in
  Printf.fprintf oc "Content-Length: %d\r\n\r\n%s" (String.length data) data;
  flush oc

let read_msg (ic : in_channel) : J.t option =
  let rec read_headers len =
    match input_line ic with
    | exception End_of_file -> None
    | line ->
      let line = String.trim line in
      if line = "" then Some len
      else
        let lower = String.lowercase_ascii line in
        if String.length lower > 15 && String.sub lower 0 15 = "content-length:" then
          read_headers (int_of_string (String.trim (String.sub line 15 (String.length line - 15))))
        else read_headers len
  in
  match read_headers 0 with
  | None | Some 0 -> None
  | Some len -> ( try Some (J.from_string (really_input_string ic len)) with _ -> None)

let jstr s : J.t = `String s
let jint i : J.t = `Int i

let request id meth params : J.t =
  `Assoc [ ("jsonrpc", jstr "2.0"); ("id", jint id); ("method", jstr meth); ("params", params) ]

let notification meth params : J.t =
  `Assoc [ ("jsonrpc", jstr "2.0"); ("method", jstr meth); ("params", params) ]

let text_doc uri : J.t = `Assoc [ ("uri", jstr uri) ]
let position line character : J.t = `Assoc [ ("line", jint line); ("character", jint character) ]

let doc_pos uri line character : J.t =
  `Assoc [ ("textDocument", text_doc uri); ("position", position line character) ]

let () =
  let argv = Sys.argv in
  if Array.length argv < 9 then begin
    prerr_endline "usage: lsp_client SERVER FILE hl hc cl cc sl sc";
    exit 2
  end;
  let server = argv.(1) and file = argv.(2) in
  let hl = int_of_string argv.(3) and hc = int_of_string argv.(4) in
  let cl = int_of_string argv.(5) and cc = int_of_string argv.(6) in
  let sl = int_of_string argv.(7) and sc = int_of_string argv.(8) in
  let abs_file =
    if Filename.is_relative file then Filename.concat (Sys.getcwd ()) file else file
  in
  let uri = "file://" ^ abs_file in
  let text = In_channel.with_open_bin abs_file In_channel.input_all in
  let ic, oc = Unix.open_process server in
  send oc
    (request 1 "initialize"
       (`Assoc
          [
            ("processId", `Null);
            ("rootUri", jstr ("file://" ^ Filename.dirname abs_file));
            ( "capabilities",
              `Assoc [ ("general", `Assoc [ ("positionEncodings", `List [ jstr "utf-8" ]) ]) ] );
          ]));
  (* wait for the initialize response *)
  (match read_msg ic with
  | Some resp ->
    let caps = member "capabilities" (member "result" resp) in
    let interesting =
      [ "hoverProvider"; "definitionProvider"; "documentSymbolProvider"; "positionEncoding" ]
    in
    let shown =
      List.filter_map
        (fun k -> match member k caps with `Null -> None | v -> Some (k ^ "=" ^ J.to_string v))
        interesting
    in
    let has k = match member k caps with `Null -> "no" | _ -> "yes" in
    print_endline ("CAPS: " ^ String.concat " " shown);
    print_endline ("CAPS2: completion=" ^ has "completionProvider" ^ " signatureHelp=" ^ has "signatureHelpProvider")
  | None ->
    print_endline "no initialize response";
    exit 1);
  send oc (notification "initialized" (`Assoc []));
  send oc
    (notification "textDocument/didOpen"
       (`Assoc
          [
            ( "textDocument",
              `Assoc
                [
                  ("uri", jstr uri); ("languageId", jstr "c0"); ("version", jint 1);
                  ("text", jstr text);
                ] );
          ]));
  send oc (request 2 "textDocument/hover" (doc_pos uri hl hc));
  send oc (request 3 "textDocument/definition" (doc_pos uri hl hc));
  send oc
    (request 4 "textDocument/documentSymbol" (`Assoc [ ("textDocument", text_doc uri) ]));
  send oc (request 5 "textDocument/completion" (doc_pos uri cl cc));
  send oc (request 6 "textDocument/signatureHelp" (doc_pos uri sl sc));
  send oc (request 99 "shutdown" `Null);
  send oc (notification "exit" `Null);
  let seen_diags = ref false in
  let continue = ref true in
  while !continue do
    match read_msg ic with
    | None -> continue := false
    | Some msg -> (
      match member "method" msg with
      | `String "textDocument/publishDiagnostics" ->
        if not !seen_diags then begin
          seen_diags := true;
          let diags = to_list (member "diagnostics" (member "params" msg)) in
          Printf.printf "DIAGNOSTICS: %d\n" (List.length diags);
          List.iter
            (fun d ->
              let range = member "range" d in
              let start = member "start" range in
              let line = match member "line" start with `Int i -> i | _ -> -1 in
              let msg' = to_string_default "?" (member "message" d) in
              let first_line =
                match String.index_opt msg' '\n' with
                | Some i -> String.sub msg' 0 i
                | None -> msg'
              in
              Printf.printf "  line %d: %s\n" line first_line)
            diags
        end
      | _ -> (
        match member "id" msg with
        | `Int 2 ->
          let contents = member "contents" (member "result" msg) in
          let value = to_string_default "(none)" (member "value" contents) in
          let value = String.concat "\\n" (String.split_on_char '\n' value) in
          Printf.printf "HOVER: %s\n" value
        | `Int 3 -> (
          match to_list (member "result" msg) with
          | loc :: _ ->
            let u = to_string_default "?" (member "uri" loc) in
            let start = member "start" (member "range" loc) in
            let line = match member "line" start with `Int i -> i | _ -> -1 in
            Printf.printf "DEFINITION: %s line %d\n" (Filename.basename u) line
          | [] -> print_endline "DEFINITION: none")
        | `Int 4 ->
          let syms = to_list (member "result" msg) in
          Printf.printf "SYMBOLS: %s\n"
            (String.concat ", "
               (List.map (fun s -> to_string_default "?" (member "name" s)) syms))
        | `Int 5 ->
          let result = member "result" msg in
          let items = match result with `List l -> l | _ -> to_list (member "items" result) in
          let labels = List.map (fun i -> to_string_default "?" (member "label" i)) items in
          let first = List.filteri (fun i _ -> i < 10) labels in
          Printf.printf "COMPLETION (%d): %s\n" (List.length labels) (String.concat ", " first)
        | `Int 6 ->
          let sigs = to_list (member "signatures" (member "result" msg)) in
          List.iter
            (fun s -> Printf.printf "SIGNATURE: %s\n" (to_string_default "?" (member "label" s)))
            sigs;
          if sigs = [] then print_endline "SIGNATURE: none"
        | `Int 99 -> continue := false
        | _ -> ()))
  done;
  match Unix.close_process (ic, oc) with
  | Unix.WEXITED n -> Printf.printf "SERVER EXIT: %d\n" n
  | _ -> print_endline "SERVER EXIT: signal"
