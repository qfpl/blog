{ mkDerivation, aeson, base, hakyll, latex-formulae-hakyll
, latex-formulae-image, latex-formulae-pandoc, pandoc, scientific
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "blog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base hakyll latex-formulae-hakyll latex-formulae-image
    latex-formulae-pandoc pandoc scientific text unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
