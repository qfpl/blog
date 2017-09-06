{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in
  import ( pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "reflex-tutorial";
    rev = "ef96b0efa65cecb7539d4b15b3a9c3f274de99f8";
    sha256 = "04b09mc9j88sgg71whm3kyb4xy0pljgk7a910zrv6d5pnx7hxnm6";
  })
