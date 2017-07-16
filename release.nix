let
  pkgs = import <nixpkgs> {};

  # Import the nix package for our site generator
  generator = import ./generator/default.nix;
  # Import the nix package for our generated site
  blog = import ./content/default.nix;

  jobs = rec {
    inherit generator;
    inherit blog;
  };
in
  jobs
