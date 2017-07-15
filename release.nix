let
  pkgs = import <nixpkgs> {};

  # Import the nix package for our blog 
  blog = import ./content/default.nix;

  jobs = rec {
    inherit blog;
  };
in
  jobs
