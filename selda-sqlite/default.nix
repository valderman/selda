{ mkDerivation, base, direct-sqlite, exceptions, selda, stdenv
, text
}:
mkDerivation {
  pname = "selda-sqlite";
  version = "0.1.7.0";
  src = ./.;
  libraryHaskellDepends = [
    base direct-sqlite exceptions selda text
  ];
  homepage = "https://valderman.github.io/selda";
  description = "SQLite backend for the Selda database EDSL";
  license = stdenv.lib.licenses.mit;
}
