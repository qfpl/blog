{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "5c4e070fb622b3188949c1f37d6cdb19b65dc615";
    sha256 = "0j68cr136yi7p9cj9bsals7jmsizhxn7iisn9lg0wj3magz203yi";
  })



