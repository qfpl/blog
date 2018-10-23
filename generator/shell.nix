{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc802"
}:

let
  inherit (nixpkgs) pkgs;
  site = import ./default.nix { inherit nixpkgs compiler; };
in
  if pkgs.lib.inNixShell then site.env else site
