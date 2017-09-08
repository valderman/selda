{ mkDerivation, base, exceptions, hashable, mtl, psqueues, stdenv
, text, time, unordered-containers
}:
mkDerivation {
  pname = "selda";
  version = "0.1.11.0";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions hashable mtl psqueues text time
    unordered-containers
  ];
  homepage = "https://github.com/valderman/selda";
  description = "Type-safe, high-level EDSL for interacting with relational databases";
  license = stdenv.lib.licenses.mit;
}
