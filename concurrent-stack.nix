{ mkDerivation, fetchgit, async, base, HUnit, protolude, stdenv, stm
, test-framework, test-framework-hunit, transformers
}:
mkDerivation {
  pname = "concurrent-stack";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/rlupton20/concurrent-stack";
    sha256 = "1lrljdikfsjhf72iqpayd62sbp4xgcxiyfnldq4gs2g3a5ivv69g";
    rev = "952b282e1907ff06a39e3778404eab5b2228f58c";
  };
  libraryHaskellDepends = [ async base protolude stm transformers ];
  testHaskellDepends = [
    base HUnit protolude test-framework test-framework-hunit
  ];
  homepage = "https://github.com/githubuser/concurrent-stack#readme";
  license = stdenv.lib.licenses.lgpl3;
}
