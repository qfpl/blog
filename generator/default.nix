{ compiler ? "default"
, nixpkgs ? import <nixpkgs> {}
}:
let
  haskellPackages = if compiler == "default"
             then nixpkgs.haskellPackages
             else nixpkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      latex-formulae-image = nixpkgs.haskell.lib.doJailbreak super.latex-formulae-image;
      latex-formulae-pandoc = nixpkgs.haskell.lib.doJailbreak super.latex-formulae-pandoc;
      latex-formulae-hakyll = nixpkgs.haskell.lib.doJailbreak super.latex-formulae-hakyll;
      foundation = nixpkgs.haskell.lib.dontCheck super.foundation;
      skylighting = nixpkgs.haskell.lib.dontCheck super.skylighting;
    };
  };

  blog = modifiedHaskellPackages.callPackage ./blog-generator.nix {};
in
  blog
