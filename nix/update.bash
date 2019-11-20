#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git

nix-prefetch-git https://github.com/NixOS/nixpkgs > nixpkgs.json
nix-prefetch-git \
    https://github.com/qfpl/growing-a-datepicker > \
    growing-a-datepicker.json
nix-prefetch-git \
    https://github.com/qfpl/reflex-tutorial > \
    reflex-tutorial.json
