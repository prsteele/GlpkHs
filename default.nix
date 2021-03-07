{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc884"
}:
pkgs.haskell.packages.${compiler}.callPackage ./nix/glpk-headers-haskell.nix { }
