{ mkDerivation, base, bytestring, exceptions, mtl, text, time, containers, random, uuid-types, stdenv
}:
mkDerivation {
  pname = "selda";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring exceptions mtl text time containers random uuid-types
  ];
  homepage = "https://valderman.github.io/selda";
  description = "Type-safe, high-level EDSL for interacting with relational databases";
  license = stdenv.lib.licenses.mit;
}
