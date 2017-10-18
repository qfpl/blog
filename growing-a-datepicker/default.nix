{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner  = "qfpl";
    repo   = "growing-a-datepicker";
    rev    = "95b4295df85b8d9082db3fed662071f3dcc477a9";
    sha256 = "08ixfi7wja9mvai3zs6hswhfgg9xrl8picflfi8042smr5n9j6vk";
  })


