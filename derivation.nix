{ mkDerivation, ansi-terminal, base, containers, directory
, equivalence, filepath, haskeline, mtl, neat-interpolation
, optparse-applicative, parsers, prettyprinter, smallcheck, stdenv
, tasty, tasty-hunit, tasty-smallcheck, text, transformers
, trifecta, unordered-containers
}:
mkDerivation {
  pname = "myml";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers equivalence mtl parsers prettyprinter smallcheck
    transformers trifecta unordered-containers
  ];
  executableHaskellDepends = [
    ansi-terminal base containers directory equivalence filepath
    haskeline mtl neat-interpolation optparse-applicative parsers
    prettyprinter smallcheck text transformers trifecta
    unordered-containers
  ];
  testHaskellDepends = [
    base containers equivalence mtl parsers prettyprinter smallcheck
    tasty tasty-hunit tasty-smallcheck transformers trifecta
    unordered-containers
  ];
  homepage = "https://github.com/linyinfeng/myml#readme";
  description = "My toy programming language(WIP)";
  license = stdenv.lib.licenses.mit;
}
