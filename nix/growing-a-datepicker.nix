{ nixpkgs }:
let
  json = builtins.fromJSON (builtins.readFile ./growing-a-datepicker.json);
  source = import (builtins.fetchTarball (with json; {
    url = "${url}/archive/${rev}.tar.gz";
    inherit sha256;
  }));
in
source { inherit nixpkgs; }
