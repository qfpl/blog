{ nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;

  reflex-tutorial = import <reflex-tutorial> {};

  # Import the nix package for our site generator
  generator = import ./generator;
  # Import the nix package for our generated site
  blog = import ./content { inherit generator reflex-tutorial; };

  jobs = rec {
    inherit generator;
    inherit reflex-tutorial;
    inherit blog;
  };
in
  jobs
