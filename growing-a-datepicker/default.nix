{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "676c304cd2449e48165c3aa6324a01b67e9bfcdc";
    sha256 = "1ja3rwczzzyj1kyvp601ww9rcpb6kcyhyg9lpfc33ba6sndjds0g";
  })



