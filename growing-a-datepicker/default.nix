{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "a5f3c0e046291b5229d623cb68fb865ddc2ae724";
    sha256 = "04a2116lnk5lf7j58lqpc3nagggaywf253cw1n6h4sjc4r1a40cx";
  })

