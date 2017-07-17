let
  pkgs = import <nixpkgs> {};

  modifiedHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      latex-formulae-image = pkgs.haskell.lib.doJailbreak super.latex-formulae-image;
      latex-formulae-pandoc = pkgs.haskell.lib.doJailbreak super.latex-formulae-pandoc;
      latex-formulae-hakyll = pkgs.haskell.lib.doJailbreak super.latex-formulae-hakyll;
    };
  };

  blog = modifiedHaskellPackages.callPackage ./blog-generator.nix {};
in
  blog
