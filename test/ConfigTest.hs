{-# LANGUAGE OverloadedStrings #-}
module ConfigTest
( configTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Data.Yaml as Y
import Data.ByteString as B

import Config
import Config.Internal


configTest :: TF.Test
configTest = testGroup "Config.hs tests" $ hUnitTestToTests $ 
             HU.TestList [ testCanParseNetConfig ]


testCanParseNetConfig :: HU.Test
testCanParseNetConfig = "Can parse YAML for network information" ~: test
  where
    test = let yaml = "device: device\naddress: address"
               parsed = Y.decode yaml
               expected = Just $ NetConfig "device" "address" in
           expected @=? parsed


