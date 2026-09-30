let version = "0.1.0"

let usage () =
  print_string
    {|c0ls — language server for C0

Usage:
  c0ls                 start the LSP server on stdin/stdout
  c0ls server          same as above
  c0ls check FILE...   print diagnostics for the given .c0/.c1/.h0 files
  c0ls --version       print the version

The LSP server is meant to be launched by an editor (Neovim, Vim, Helix,
Emacs, VS Code, ...). See the README for per-editor configuration.
|}

let () =
  match Array.to_list Sys.argv with
  | [ _ ] | [ _; "server" ] -> C0_lsp.Server.run ()
  | _ :: "check" :: files ->
    if files = [] then begin
      usage ();
      exit 2
    end
    else exit (C0_lsp.Cli.check_files files)
  | [ _; "--version" ] | [ _; "-V" ] -> Printf.printf "c0ls %s\n" version
  | [ _; "--help" ] | [ _; "-h" ] -> usage ()
  | _ ->
    usage ();
    exit 2
