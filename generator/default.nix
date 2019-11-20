{ compiler ? "default"
, nixpkgs ? import ../nix/nixpkgs.nix
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
             then pkgs.haskellPackages
             else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      latex-formulae-image = overrideCabal (super.callHackageDirect {
        pkg = "latex-formulae-image";
        ver = "0.1.1.4";
        sha256 = "05y6a9ywl7q9c5dnxckvq7ij113wbmdrvmirlfx8h2yd0n504b7m";
      } {}) (_: {
        revision = "1";
        editedCabalFile = "0m15dyrxala5dh5sh228bsrfa3nym9wd719byj19vfl4i2nabw9d";
      });
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
