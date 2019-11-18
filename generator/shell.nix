{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "ghc844"
}:

let
  inherit (nixpkgs) pkgs;
  site = import ./default.nix { inherit nixpkgs compiler; };
in
  if pkgs.lib.inNixShell then site.env else site
