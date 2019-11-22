let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs;

  compiler = "default";

  reflex-tutorial = import <reflex-tutorial>;
  growing-a-datepicker = import <growing-a-datepicker> { inherit nixpkgs; };

  # Import the nix package for our site generator
  generator = import ./generator { inherit nixpkgs compiler; };

  # Import the nix package for our generated site
  blog = import ./content {
    inherit generator reflex-tutorial growing-a-datepicker;
  };

  jobs = { inherit blog generator; };
in
  jobs
