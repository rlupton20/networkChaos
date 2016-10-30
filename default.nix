{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let

  inherit (nixpkgs) pkgs ;

  tools = with pkgs; with pkgs.haskellPackages; [
  cabal-install stack ghc happy ghc-mod
  hindent hlint hasktags hoogle
  stylish-haskell structured-haskell-mode
  ];

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  treeThreads = haskellPackages.callPackage ./tree-threads.nix {};
  drv = haskellPackages.callPackage ./vanguard.nix { additionalTools = tools; 
                                                     tree-threads = treeThreads; };

in

  if pkgs.lib.inNixShell then drv.env else drv
