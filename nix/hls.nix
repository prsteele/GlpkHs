# Create a shell with all dependencies and haskell-language-server available.
#
# We accomplish this by swapping out mkDerivation for one that appends
# haskell-language-server to system dependencies.
#
{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc884"
}:
let
  addHls = x: x ++ [ pkgs.haskellPackages.haskell-language-server ];
  mkDerivation =
    args@{ librarySystemDepends ? [], ... }:
    pkgs.haskell.packages.${compiler}.mkDerivation
      ( args // { librarySystemDepends = addHls librarySystemDepends; } );
in
(pkgs.haskell.packages.${compiler}.callPackage ./glpk-headers-haskell.nix { inherit mkDerivation; }).env
