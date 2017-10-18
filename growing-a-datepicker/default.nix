{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "cb2bbedd69ac8a41970cb027878fe1fdca665e0b";
    sha256 = "09fl42l9fgzhx3c4p5a8ym9myrv2wwl8xzl8pdrf9p9w87c044dy";
  })
