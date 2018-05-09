{ nixpkgs ? "default"
, compiler ? "ghc802"
}:
let

  systemPkgs = import <nixpkgs> {};
  defaultPkgs = import (systemPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "bca2ee28db44bc2d2d1b5e97866ca4052d88e94f";
    sha256 = "0l9jpnkvjcj9p181ns9g4gm57n3ni4q07ynkh80r6d6qidw8cwnq";
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
