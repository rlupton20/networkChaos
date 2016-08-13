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

  drv = haskellPackages.callPackage ./vanguard.nix { additionalTools = tools; };

in

  if pkgs.lib.inNixShell then drv.env else drv
