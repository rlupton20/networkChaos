{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let

  inherit (nixpkgs) pkgs ;

  ghc = haskellPackages.ghcWithHoogle(packages: with packages; [
          cabal-install happy ghc-mod
          hindent hlint hasktags 
          stylish-haskell structured-haskell-mode
          ]);

  tools = [ ghc pkgs.stack ];

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  treeThreads = haskellPackages.callPackage ./tree-threads.nix {};
  vanguardCore = haskellPackages.callPackage ./vanguard-core.nix {};
  vanguard = haskellPackages.callPackage ./vanguard.nix { additionalTools = tools; 
                                                          tree-threads = treeThreads;
                                                          vanguard-core = vanguardCore; };

in

  if pkgs.lib.inNixShell then vanguard.env else vanguard
