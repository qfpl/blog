{ nixpkgs }:
let
  json = builtins.fromJSON (builtins.readFile ./source.json);
  source = import (builtins.fetchTarball (with json; {
    url = "${url}/archive/${rev}.tar.gz";
    inherit sha256;
  }));
in
source { inherit nixpkgs; }
