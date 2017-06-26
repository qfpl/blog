{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      latex-formulae-image = pkgs.haskell.lib.doJailbreak super.latex-formulae-image;
      latex-formulae-pandoc = pkgs.haskell.lib.doJailbreak super.latex-formulae-pandoc;
      latex-formulae-hakyll = pkgs.haskell.lib.doJailbreak super.latex-formulae-hakyll;
    };
  };

  mytexlive = (pkgs.texlive.combine {
    inherit (pkgs.texlive) 
    prftree
    scheme-basic
    collection-latexrecommended 
    collection-mathextra;
    });

  blog = modifiedHaskellPackages.callPackage ./. {};
  blogWithDeps = pkgs.haskell.lib.overrideCabal blog (drv: {
    executableHaskellDepends = [ mytexlive pkgs.imagemagick pkgs.ghostscript ] ++ drv.executableHaskellDepends;
  });

in

  if pkgs.lib.inNixShell then blogWithDeps.env else blogWithDeps
