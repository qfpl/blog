{ compiler ? "ghc844"
, nixpkgs ? import ../nix/nixpkgs.nix
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
             then pkgs.haskellPackages
             else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      latex-formulae-image =
        unmarkBroken (doJailbreak super.latex-formulae-image);
      latex-formulae-pandoc =
        unmarkBroken (doJailbreak super.latex-formulae-pandoc);
      latex-formulae-hakyll =
        unmarkBroken (doJailbreak super.latex-formulae-hakyll);

      Diff = dontCheck super.Diff;
      hakyll = doJailbreak super.hakyll;
      system-fileio = doJailbreak super.system-fileio;
    };
  };

  blog = modifiedHaskellPackages.callPackage ./blog-generator.nix {};
in
  blog
