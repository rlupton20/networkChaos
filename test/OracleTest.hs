{-# LANGUAGE OverloadedStrings #-}
module OracleTest
( oracleTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Oracle.API
import Oracle.API.Internal
import Config.Types

oracleTest :: TF.Test
oracleTest = testGroup "Oracle API tests" $ hUnitTestToTests $ HU.TestList [testCreateOracleFromConfig]


testCreateOracleFromConfig :: HU.Test
testCreateOracleFromConfig = "test an oracle can be created from oracle configuration object" ~: test
  where
    test = let config = OracleConfig "address" "auth.cert" in
      do
        oracle <- makeOracle config
        let (Oracle address) = oracle
        "address" @=? address
        


mockOracle :: IO ()
mockOracle = undefined