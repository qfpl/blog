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

  # Import the nix package for our Hakyll site-generator
  site = modifiedHaskellPackages.callPackage ./. {};

  siteWithDeps = pkgs.haskell.lib.overrideCabal site (drv: {
    executableHaskellDepends = [ mytexlive pkgs.imagemagick pkgs.ghostscript ] ++ drv.executableHaskellDepends;
  });


  # A dummy activate script so we can use hail for deployment
  activate = pkgs.writeScriptBin "activate" 
    ''
      #!${pkgs.bash}/bin/bash -e
    '';

  jobs = rec {

    blog = pkgs.haskell.lib.overrideCabal site (drv: {
      postInstall = ''
        $out/bin/site build

        # Make the generated blog part of the output
        mkdir -p $out/blog
        cp -r _site/* $out/blog

        # Testing hydra build products
        mkdir -p $out/nix-support
        echo "Hello" > $out/text.txt
        echo "doc none $out/text.txt" > $out/nix-support/hydra-build-products

        # Clear out the outputs that we don't want
        rm -Rf $out/bin/*
        rm -Rf $out/share

        # Setup the activate script for hail to use
        ln -sv ${activate}/bin/activate $out/bin
      '';
    });

  };
in
  jobs
