{ pkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
let
  hps = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: with pkgs.haskell.lib; {
      selda = self.callPackage ./selda {};
      selda-postgresql = self.callPackage ./selda-postgresql {};
      selda-sqlite = self.callPackage ./selda-sqlite {};
      selda-tests = dontHaddock (self.callPackage ./selda-tests {});
    };
  };
in with hps; {
    inherit selda selda-postgresql selda-tests selda-sqlite;
  }