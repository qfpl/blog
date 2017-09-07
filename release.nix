{ nixpkgs ? import <nixpkgs> {}
, reflex-tutorial ? import ./reflex-tutorial
}:
let
  inherit (nixpkgs) pkgs;

  # Import the nix package for our site generator
  generator = import ./generator;
  # Import the nix package for our generated site
  blog = import ./content {
    generator = generator;
    reflex-tutorial = reflex-tutorial {};
  };

  jobs = rec {
    inherit generator;
    inherit reflex-tutorial;
    inherit blog;
  };
in
  jobs
