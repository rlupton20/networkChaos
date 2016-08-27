{-# LANGUAGE OverloadedStrings #-}
module OracleTest
( oracleHTTPSTest ) where

import qualified Test.Framework as TF
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Test.HUnit as HU
import           Test.HUnit ((~:),(@=?))

import Oracle.API
import Oracle.API.Internal
import Config.Internal

oracleHTTPSTest :: TF.Test
oracleHTTPSTest = TF.testGroup "HTTPS Oracle tests" [oracleHTTPSUnitTests, oracleHTTPSIntegrationTests]

oracleHTTPSUnitTests :: TF.Test
oracleHTTPSUnitTests = TF.testGroup "Oracle HTTPS unit tests" $ hUnitTestToTests $
  HU.TestList [ testGetOracleHTTPSSignedCertificateAuthentication ]

oracleHTTPSIntegrationTests :: TF.Test
oracleHTTPSIntegrationTests = TF.testGroup "Oracle HTTPS integration tests" $ hUnitTestToTests $
  HU.TestList [ testClientCertHookFindsCertificates ]


testGetOracleHTTPSSignedCertificateAuthentication = "test we can extract signed certificate details from Oracle Config" ~: test
  where
    test = let config = OracleHTTPS undefined undefined (CertID $ AuthenticationCertificate "cert" "key")
               credentials = getHTTPSCertificates config
              in
            Just ("cert", "key") @=? credentials

  
testClientCertHookFindsCertificates = "test we can load a signed certificate and key from disk" ~: test
  where
    test = let config = OracleHTTPS undefined undefined (CertID $ AuthenticationCertificate "app/test/mycert.cert" "app/test/mykey.key")
           in
         do
           credentials <- clientCertHook config undefined
           let isAJust = maybe False (const True) credentials
           True @=? isAJust           
