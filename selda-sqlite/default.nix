{ mkDerivation, base, direct-sqlite, exceptions, selda, stdenv
, text
}:
mkDerivation {
  pname = "selda-sqlite";
  version = "0.1.6.0";
  src = ./.;
  libraryHaskellDepends = [
    base direct-sqlite exceptions selda text
  ];
  homepage = "https://github.com/valderman/selda";
  description = "SQLite backend for the Selda database EDSL";
  license = stdenv.lib.licenses.mit;
}
