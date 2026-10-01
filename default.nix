# Convenience entry point for non-flake users: `nix-build` / `nix-shell`.
{
  pkgs ? import <nixpkgs> { },
}:

pkgs.callPackage ./package.nix { }
