{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "8f11c7ccff5fa3f7ab6dbca4c1154856dd14c15c";
    sha256 = "1wwgs49r90mavzm5ardq063yjhmlcaifbd7lrn9ai84n15145r00";
  })



