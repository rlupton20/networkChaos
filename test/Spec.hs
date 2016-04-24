import Test.Framework
import Test.Framework.Providers.HUnit


import TypesTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Types.hs" $ hUnitTestToTests typesTest]