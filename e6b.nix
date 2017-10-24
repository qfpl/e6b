{ mkDerivation, ansi-wl-pprint, base, digit, hedgehog, lens
, optparse-applicative, papa, parsec, parsec-numbers, parsers
, pretty, stdenv, tasty, tasty-hedgehog, tasty-hspec, tasty-hunit
, text
}:
mkDerivation {
  pname = "e6b";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lens papa ];
  executableHaskellDepends = [
    base digit lens optparse-applicative papa parsec parsec-numbers
    parsers
  ];
  testHaskellDepends = [
    ansi-wl-pprint base hedgehog papa parsec parsers pretty tasty
    tasty-hedgehog tasty-hspec tasty-hunit text
  ];
  homepage = "https://github.com/qfpl/e6b";
  description = "E6B Flight Computer functions";
  license = stdenv.lib.licenses.bsd3;
}
