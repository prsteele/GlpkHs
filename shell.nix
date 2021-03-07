{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc884"
}:
(import ./default.nix { inherit sources pkgs compiler; }).env
