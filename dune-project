(lang dune 3.0)
(cram enable)
(name c0ls)

(generate_opam_files true)

(package
 (name c0ls)
 (synopsis "Language server for the C0 programming language")
 (description
  "An editor-agnostic LSP server for C0 (the safe C subset taught in CMU
15-122), usable from Vim, Neovim, Helix, Emacs, VS Code, or any other
LSP-capable editor. Provides diagnostics (parse and type errors), hover,
go-to-definition, completion, signature help and document symbols,
including support for #use libraries and multi-file projects driven by
README.txt / project.txt files.")
 (depends
  (ocaml (>= 4.14))
  dune
  yojson
  lwt
  linol
  linol-lwt))
