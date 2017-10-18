{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "5abdf9f60098237d85d2618ae19eca52af1ef4eb";
    sha256 = "1swqanb2ga80fj83ddcjl4z9awfibfk1ayid1wc8lslh8zraflay";
  })

