#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git

nix-prefetch-git \
    https://github.com/NixOS/nixpkgs > \
    ../nixpkgs/source.json
nix-prefetch-git \
    https://github.com/qfpl/growing-a-datepicker > \
    ../growing-a-datepicker/source.json
nix-prefetch-git \
    https://github.com/qfpl/reflex-tutorial > \
    ../reflex-tutorial/source.json
