{ mkDerivation, ansi-wl-pprint, base, exitcode, hedgehog, lens
, papa, parsec, parsers, pretty, stdenv, tasty, tasty-hedgehog
, tasty-hspec, tasty-hunit, text
}:
mkDerivation {
  pname = "e6b";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base exitcode lens papa ];
  executableHaskellDepends = [ base exitcode lens papa ];
  testHaskellDepends = [
    ansi-wl-pprint base hedgehog papa parsec parsers pretty tasty
    tasty-hedgehog tasty-hspec tasty-hunit text
  ];
  homepage = "https://github.com/qfpl/e6b";
  description = "E6B Flight Computer functions";
  license = stdenv.lib.licenses.bsd3;
}
