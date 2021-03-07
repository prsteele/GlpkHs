{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc884"
}:
let
  shell = import ../shell.nix { inherit sources pkgs compiler; };
in
pkgs.mkShell
  { inputsFrom = [ shell ];
    buildInputs = [ pkgs.cabal2nix ];
  }
