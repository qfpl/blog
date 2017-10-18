{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "8e3894410961a11c788006e5d87d00e11ee30bf5";
    sha256 = "1bnk66lf40wzb7k4d2lk6szvk8kkw86g9jzh2dkywys501nww9gj";
  })
