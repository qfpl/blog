{ nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;

  reflex-tutorial = import <reflex-tutorial>;
  growing-a-datepicker = import <growing-a-datepicker>;

  # Import the nix package for our site generator
  generator = import ./generator;
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
