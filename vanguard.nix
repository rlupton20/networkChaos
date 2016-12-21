{ mkDerivation, mbedtls
, async, base, bytestring, cereal, connection
, containers, data-default-class, http-client, http-client-tls
, HUnit, mtl, network, network-house, QuickCheck, stdenv, stm
, stunclient, test-framework, test-framework-hunit
, test-framework-quickcheck2, time, tls, transformers
, unbounded-delays, unix, unix-bytestring, yaml, wai, warp
, http-types, tree-threads 
, additionalTools ? []
}:
mkDerivation {
  pname = "vanguard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = additionalTools;
  executableHaskellDepends = [
    # System dependencies
    mbedtls
    # Haskell dependencies
    async base bytestring cereal connection containers
    data-default-class http-client http-client-tls mtl network
    network-house stm stunclient tls transformers unbounded-delays unix
    unix-bytestring yaml wai warp tree-threads
  ];
  testHaskellDepends = [
    # System dependencies
    mbedtls
    # Haskell dependencies
    async base bytestring connection containers data-default-class
    http-client http-client-tls HUnit mtl QuickCheck stm test-framework
    test-framework-hunit test-framework-quickcheck2 time tls
    transformers yaml http-types tree-threads
  ];
  homepage = "https://github.com/rlupton20/networkChaos";
  description = "A peer-to-peer VPN";
  license = stdenv.lib.licenses.gpl3;
}
