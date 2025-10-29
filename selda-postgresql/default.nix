{ mkDerivation, base, bytestring, exceptions, postgresql-libpq
, selda, stdenv, text
}:
mkDerivation {
  pname = "selda-postgresql";
  version = "0.1.8.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring exceptions postgresql-libpq selda text
  ];
  homepage = "https://valderman.github.io/selda";
  description = "PostgreSQL backend for the Selda database EDSL";
  license = stdenv.lib.licenses.mit;
}
