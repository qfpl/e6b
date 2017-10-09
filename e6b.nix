{ mkDerivation, base, checkers, exitcode, lens, papa, QuickCheck
, stdenv, tasty, tasty-hunit, tasty-quickcheck
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
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/e6b";
  description = "E6B Flight Computer functions";
  license = stdenv.lib.licenses.bsd3;
}
