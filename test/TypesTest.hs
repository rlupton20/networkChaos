module TypesTest
( typesTest ) where

import Test.HUnit

import Types

type Label = String
type Input = String
type Expected a = a

addrConversionTest :: (String, String, Expected (Maybe Addr)) -> Test
addrConversionTest (label, input, expected) = label ~: (addr input :: Maybe Addr) @=? expected

addrConversionRuns = [ ("addr: doesn't convert \"String\"", 	   "String", Nothing)
		     , ("addr: addr and addrW8 agree", 		   "1.2.34.123", Just $ addrW8 1 2 34 123)
		     , ("addr: fail on \"1.43.t.90\"",		   "1.43.t.90", Nothing) 
		     , ("addr: fails on \"...\"",		   "...", Nothing) ]

addrTests :: Test
addrTests = TestLabel "addr tests" $ TestList $ map addrConversionTest addrConversionRuns

typesTest :: Test
typesTest = TestList [addrTests]