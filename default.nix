let
  pkgs = import <nixpkgs> {};

  mytexlive = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
    prftree
    scheme-basic
    collection-latexrecommended
    collection-mathextra;
    });

  modifiedHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      latex-formulae-image = pkgs.haskell.lib.doJailbreak super.latex-formulae-image;
      latex-formulae-pandoc = pkgs.haskell.lib.doJailbreak super.latex-formulae-pandoc;
      latex-formulae-hakyll = pkgs.haskell.lib.doJailbreak super.latex-formulae-hakyll;
    };
  };

  # A dummy activate script so we can use hail for deployment
  activate = pkgs.writeScriptBin "activate"
    ''
      #!${pkgs.bash}/bin/bash -e
    '';

  # Import the nix package for our Hakyll site-generator
  site = modifiedHaskellPackages.callPackage ./site.nix {};

  # Set things up so that nix-build will create a `blog` directory containing the site
  siteWithDeps = pkgs.haskell.lib.overrideCabal site (drv: {
    executableHaskellDepends = [ mytexlive pkgs.imagemagick pkgs.ghostscript ] ++ drv.executableHaskellDepends;
    postInstall = ''
      $out/bin/site build

      # Make the generated blog part of the output
      mkdir -p $out/blog
      cp -r _site/* $out/blog

      # Clear out the outputs that we don't want
      rm -Rf $out/bin/*
      rm -Rf $out/share

      # Setup the activate script for hail to use
      ln -sv ${activate}/bin/activate $out/bin
    '';
  });
in
  siteWithDeps
