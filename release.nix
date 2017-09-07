{ nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;

  reflex-tutorial-src = import <reflex-tutorial> {};
  reflex-tutorial = import ${reflex-tutorial-src}/ {};

  # Import the nix package for our site generator
  generator = import ./generator;
  # Import the nix package for our generated site
  blog = import ./content { inherit generator reflex-tutorial; };

  jobs = rec {
    inherit generator;
    inherit blog;
  };
in
  jobs
