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
import Config.Types


configTest :: TF.Test
configTest = testGroup "Config.hs tests" $ hUnitTestToTests $ HU.TestList [ testCanParseNetConfig
                                                                          , testCanParseBootstrap ]


testCanParseNetConfig :: HU.Test
testCanParseNetConfig = "Can parse YAML for network information" ~: test
  where
    test = let yaml = "device: device\naddress: address"
               parsed = Y.decode yaml
               expected = Just $ NetConfig "device" "address" in
             do expected @=? parsed


testCanParseBootstrap :: HU.Test
testCanParseBootstrap = "Can parse YAML for bootstrap information" ~: test
  where test = let yaml = "address: test\nnode: auth.cert"
                   parsed = Y.decode yaml
                   expected = Just $ Bootstrap "test" "auth.cert" in
                 do expected @=? parsed
