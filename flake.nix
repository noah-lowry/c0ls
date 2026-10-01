{
  description = "Language server for the C0 programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      overlays.default = final: prev: {
        c0ls = final.callPackage ./package.nix { };
      };

      packages = forAllSystems (pkgs: rec {
        c0ls = pkgs.callPackage ./package.nix { };
        default = c0ls;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          inputsFrom = [ self.packages.${pkgs.stdenv.hostPlatform.system}.c0ls ];
          packages = with pkgs.ocamlPackages; [
            ocaml-lsp
            ocamlformat
            utop
          ];
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);
    };
}
