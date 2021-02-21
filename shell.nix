# from https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
{ compilerName ? "ghc884" }:

let

  compiler = pkgs.haskell.packages."${compilerName}";

  nixpkgsRev = "758b29b5a28b818e311ad540637a5c1e40867489";
  # pinned nixpkgs before cabal 3 becomes the default else hie fails
  # nixpkgs = import <nixpkgs>
  nixpkgs = import (builtins.fetchTarball {
      name = "nixos-unstable";
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
      sha256 = "00nk1a002zzi0ij4xp2hf7955wj49qdwsm2wy7mzbpjbgick6scp";
  })
  {
    overlays = [ overlay]; config = {allowBroken = true;};
  };
  pkgs = nixpkgs.pkgs;

  # my_pkg = (import ./. { inherit compiler; } );
in
  pkgs.mkShell {
    name = "netlink-hs";
    buildInputs = with pkgs; [
      ghc
      haskellPackages.cabal-install
      haskellPackages.c2hs
      haskellPackages.stylish-haskell
      haskellPackages.hlint
      haskellPackages.haskell-language-server
      haskellPackages.hasktags
    ];

  }
