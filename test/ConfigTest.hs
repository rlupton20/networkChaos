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
                                                                          , testCanParseOracle
                                                                          , testCanParseSignedCertificateAuthentication
                                                                          , testCanParseOracleAuthenticationConfiguration ] 


testCanParseNetConfig :: HU.Test
testCanParseNetConfig = "Can parse YAML for network information" ~: test
  where
    test = let yaml = "device: device\naddress: address"
               parsed = Y.decode yaml
               expected = Just $ NetConfig "device" "address" in
           expected @=? parsed


testCanParseOracle :: HU.Test
testCanParseOracle = "Can parse YAML for oracle information" ~: test
  where test = let yaml = "address: test\noracle-certificate: auth.cert"
                   parsed = Y.decode yaml
                   expected = Just $ OracleConfig "test" "auth.cert" in
               expected @=? parsed

testCanParseSignedCertificateAuthentication :: HU.Test
testCanParseSignedCertificateAuthentication = "Can parse YAML for oracle signed certificate authentication" ~: test
  where test = let yaml = "signed-certificate: cert\nprivate-key: key"
                   parsed = Y.decode yaml
                   expected = Just $ AuthenticationCertificate "cert" "key" in
               expected @=? parsed



testCanParseOracleAuthenticationConfiguration :: HU.Test
testCanParseOracleAuthenticationConfiguration = "Can parse YAML for oracle authentication information" ~: test
  where test = let yaml = "signed-certificate:\n  signed-certificate: cert\n  private-key: key"
                   parsed = Y.decode yaml
                   expected = Just $ CertID (AuthenticationCertificate "cert" "key") in
               expected @=? parsed
