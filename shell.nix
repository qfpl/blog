{ nixpkgs ? import <nixpkgs> {}}:
let
  inherit (nixpkgs) pkgs;
  site = import ./default.nix;
in
  if pkgs.lib.inNixShell then site.env else site
