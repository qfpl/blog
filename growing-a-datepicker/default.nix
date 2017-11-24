{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "56b2c3697dedf636f03fd1b9124623045337acb1";
    sha256 = "0b1z3jv9l99nsxb7g0271w7wfbl54qnalx52wzad9mkg2ni7yfyi";
  })
