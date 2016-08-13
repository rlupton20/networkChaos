{ pkgs ? import <nixpkgs> {} }:
with pkgs; with pkgs.haskellPackages;
mkDerivation {
  pname = "vanguard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
  cabal-install stack ghc happy ghc-mod
  hindent hlint hasktags hoogle
  stylish-haskell structured-haskell-mode
  ];
  executableHaskellDepends = [
    async base bytestring cereal connection containers
    data-default-class http-client http-client-tls mtl network
    network-house stm stunclient tls transformers unbounded-delays unix
    unix-bytestring yaml
  ];
  testHaskellDepends = [
    async base bytestring connection containers data-default-class
    http-client http-client-tls HUnit mtl QuickCheck stm test-framework
    test-framework-hunit test-framework-quickcheck2 time tls
    transformers yaml
  ];
  homepage = "https://github.com/rlupton20/networkChaos";
  description = "A peer-to-peer VPN";
  license = stdenv.lib.licenses.gpl3;
}
