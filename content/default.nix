{ nixpkgs ? import <nixpkgs> {}
, generator ? import ../generator
, reflex-tutorial ? import ../reflex-tutorial {}
, growing-a-datepicker ? import ../growing-a-datepicker {}
}:
let
  inherit (nixpkgs) pkgs;

  mytexlive = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
    prftree
    scheme-basic
    collection-latexrecommended
    collection-mathscience;
  });

  activate = pkgs.writeScriptBin "activate" ''
    #!${pkgs.bash}/bin/bash -e

    '';

in
  pkgs.stdenv.mkDerivation {
    name = "blog-content";
    src = ./.;

    unpackPhase = ''
      cp -r $src/* .
      chmod -R +w .
      mkdir -p ./drafts/reflex
      ln -sv ${reflex-tutorial}/drafts/reflex/basics ./drafts/reflex/basics
      mkdir -p ./posts/reflex
      ln -sv ${reflex-tutorial}/posts/reflex/basics ./posts/reflex/basics
      mkdir -p ./css/reflex
      ln -sv ${reflex-tutorial}/css/reflex/basics ./css/reflex/basics
      ln -sv ${reflex-tutorial}/css/reflex/basics-exercises ./css/reflex/basics-exercises
      mkdir -p ./js/reflex
      ln -sv ${reflex-tutorial}/js/reflex/basics ./js/reflex/basics
      ln -sv ${reflex-tutorial}/js/reflex/basics-exercises ./js/reflex/basics-exercises

      ln -sv ${growing-a-datepicker}/drafts/reflex/widget ./drafts/reflex/widget
      ln -sv ${growing-a-datepicker}/posts/reflex/widget ./posts/reflex/widget
      ln -sv ${growing-a-datepicker}/css/reflex/growing-a-datepicker ./css/reflex/growing-a-datepicker
      ln -sv ${growing-a-datepicker}/js/reflex/growing-a-datepicker ./js/reflex/growing-a-datepicker

    '';

    buildPhase = ''
      export LANG=en_US.UTF-8
      export LOCALE_ARCHIVE=/run/current-system/sw/lib/locale/locale-archive
      site build
    '';

    installPhase = ''
      mkdir -p $out/blog
      cp -r _site/* $out/blog/
      mkdir -p $out/bin
      ln -sv ${activate}/bin/activate $out/bin/
    '';

    phases = ["unpackPhase" "buildPhase" "installPhase"];

    buildInputs = [generator mytexlive pkgs.imagemagick pkgs.ghostscript pkgs.glibcLocales];
  }


