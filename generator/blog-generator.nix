{ mkDerivation, aeson, base, containers, filepath, hakyll
, latex-formulae-hakyll, latex-formulae-image
, latex-formulae-pandoc, pandoc, scientific, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "blog-generator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers filepath hakyll latex-formulae-hakyll
    latex-formulae-image latex-formulae-pandoc pandoc scientific text
    unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
