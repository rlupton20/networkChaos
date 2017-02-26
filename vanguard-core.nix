{ mkDerivation, base, fetchgit, protolude, HUnit, QuickCheck
, stdenv, stm, test-framework, test-framework-hunit, network
, test-framework-quickcheck2, bytestring, unix, containers
, unordered-containers, aeson
}:
mkDerivation {
  pname = "vanguard-core";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/rlupton20/vanguard-core";
    sha256 = "1y5zaaplijf9r0iz0xhcn5kgvsf2vyi7xlk79cgyyn2sz9dc8v13";
    rev = "6969790ef72a12e915efbc8817c5a4c934504112";
  };
  libraryHaskellDepends = [ 
    base protolude stm bytestring network unix aeson
    containers unordered-containers ];
  testHaskellDepends = [
    base protolude test-framework test-framework-hunit
    test-framework-quickcheck2 HUnit QuickCheck aeson
    bytestring
  ];
  homepage = "https://github.com/rlupton20/tree-threads#readme";
  description = "Create trees of independent threads which manage/use a central environment";
  license = stdenv.lib.licenses.lgpl3;
}
