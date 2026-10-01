{
  lib,
  ocamlPackages,
}:

ocamlPackages.buildDunePackage {
  pname = "c0ls";
  version = "0.1.0";

  minimalOCamlVersion = "4.14";
  duneVersion = "3";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./dune-project
      ./c0ls.opam
      ./bin
      ./c0lib
      ./lib
      ./test
    ];
  };

  # lwt and yojson are used directly (see lib/dune) but are deliberately not
  # listed here: linol/linol-lwt already propagate them, and on nixpkgs
  # revisions where ocamlPackages.yojson and the yojson behind
  # ppx_yojson_conv_lib are distinct builds, naming yojson again puts two
  # findlib definitions of it in the closure and the build fails.
  buildInputs = with ocamlPackages; [
    linol
    linol-lwt
  ];

  doCheck = true;

  meta = {
    description = "Language server for the C0 programming language";
    longDescription = ''
      An editor-agnostic LSP server for C0 (the safe C subset taught in CMU
      15-122), usable from Vim, Neovim, Helix, Emacs, VS Code, or any other
      LSP-capable editor. Provides diagnostics (parse and type errors), hover,
      go-to-definition, completion, signature help and document symbols,
      including support for #use libraries and multi-file projects driven by
      README.txt / project.txt files.
    '';
    homepage = "https://github.com/noah-lowry/c0ls";
    # No LICENSE file in the repo yet; add `license = lib.licenses.<id>;` once chosen.
    mainProgram = "c0ls";
    platforms = ocamlPackages.ocaml.meta.platforms;
  };
}
