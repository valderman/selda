{ mkDerivation, base, exceptions, hashable, mtl, psqueues, stdenv
, text, time, unordered-containers
}:
mkDerivation {
  pname = "selda";
  version = "0.3.3.1";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions hashable mtl psqueues text time
    unordered-containers
  ];
  homepage = "https://selda.link";
  description = "Type-safe, high-level EDSL for interacting with relational databases";
  license = stdenv.lib.licenses.mit;
}
