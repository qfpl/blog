let
  pkgs = import <nixpkgs> {};

  # Import the nix package for our blog 
  blog = import ./default.nix;

  jobs = rec {
    inherit blog;
  };
in
  jobs
