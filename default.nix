{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let

  inherit (nixpkgs) pkgs ;

  devel = import (builtins.fetchTarball https://github.com/rlupton20/alt-nixpkgs/archive/master.tar.gz) {};

  ghc = haskellPackages.ghcWithHoogle(packages: with packages; [
          cabal-install happy ghc-mod
          hindent hlint hasktags 
          stylish-haskell structured-haskell-mode
          ]);

  tools = [ ghc pkgs.stack ];

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  treeThreads = devel.haskellLibraries.treeThreads;
  concurrentStack = devel.haskellLibraries.concurrentStack;
  vanguardCore = devel.vanguard.vanguardCore;

  vanguard = haskellPackages.callPackage ./vanguard.nix { additionalTools = tools; 
                                                          tree-threads = treeThreads;
                                                          concurrent-stack = concurrentStack;
                                                          vanguard-core = vanguardCore; };

in

  if pkgs.lib.inNixShell then vanguard.env else vanguard
