{ mkDerivation, base, directory, exceptions, HUnit, selda
, selda-sqlite, stdenv, text, time
}:
mkDerivation {
  pname = "selda-tests";
  version = "0.1.0.0";
  src = ./.;
  testHaskellDepends = [
    base directory exceptions HUnit selda selda-sqlite text time
  ];
  description = "Tests for the Selda database DSL";
  license = stdenv.lib.licenses.mit;
}
