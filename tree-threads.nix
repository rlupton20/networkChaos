{ mkDerivation, async, base, fetchgit, HUnit, mtl, QuickCheck
, stdenv, stm, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers
}:
mkDerivation {
  pname = "tree-threads";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/rlupton20/tree-threads";
    sha256 = "1y36f551qqxaarq645z82dsv9rn4rb4x52a5rd1vpk315lfrgqcz";
    rev = "0380b68a661b73072293b6e19de298a0436d4ae6";
  };
  libraryHaskellDepends = [ async base mtl stm transformers ];
  testHaskellDepends = [
    async base HUnit mtl QuickCheck stm test-framework
    test-framework-hunit test-framework-quickcheck2 transformers
  ];
  homepage = "https://github.com/rlupton20/tree-threads#readme";
  description = "Create trees of independent threads which manage/use a central environment";
  license = stdenv.lib.licenses.lgpl3;
}
