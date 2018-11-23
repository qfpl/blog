{ nixpkgs ? "default"
, compiler ? "ghc802"
}:
let

  systemPkgs = import <nixpkgs> {};
  defaultPkgs = import (systemPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "636b2b2da96ce1765d40ed7ef0588603c52a58c1";
    sha256 = "08d6ln3m8vlzy9ybiiaf4zmv74k4ya3xgrfqh87paqf39i0777wb";
  }) {};

  pinnedpkgs =
    if nixpkgs == "default"
      then defaultPkgs
      else systemPkgs;

  inherit (pinnedpkgs) pkgs;

  reflex-tutorial = import <reflex-tutorial>;
  growing-a-datepicker = import <growing-a-datepicker>;

  # Import the nix package for our site generator
  generator = import ./generator {
    nixpkgs = pkgs;
    compiler = compiler;
  };

  # Import the nix package for our generated site
  blog = import ./content {
    inherit generator reflex-tutorial growing-a-datepicker;
  };

  jobs = rec {
    inherit generator;
    inherit blog;
  };
in
  jobs
