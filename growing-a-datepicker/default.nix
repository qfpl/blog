{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "fff4890f5164022052bc8510415619b27d1051fc";
    sha256 = "19ac7vnzn4nyp960llzffnlw6ls937byzf1cza2mmmqs2xlbbn0d";
  })
