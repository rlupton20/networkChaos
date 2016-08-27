{-# LANGUAGE OverloadedStrings #-}
module Oracle.API.Internal where

import           Control.Applicative       ( (<$>) )

import qualified Data.ByteString           as B
import           Data.String               ( fromString )
import qualified Network.HTTP.Client       as H
import           Network.HTTP.Client.TLS   ( mkManagerSettings )
import           Network.Connection        ( TLSSettings(..) )
  
import           Network.TLS               ( ClientParams(..), defaultParamsClient
                                           , Supported(..), Version(TLS12)
                                           , Shared(..) , ClientHooks(..)
                                           , HashAlgorithm(HashSHA512), SignatureAlgorithm(SignatureRSA)
                                           , CertificateType, HashAndSignatureAlgorithm, PrivKey )
import           Network.TLS.Extra.Cipher  (ciphersuite_strong)
import           Data.X509                 ( DistinguishedName, CertificateChain(..) )
import           Data.X509.File            ( readSignedObject, readKeyFile )
import           Data.X509.CertificateStore( makeCertificateStore )
import           Data.Default.Class        ( def ) 

import qualified Config as C


getHTTPSCertificates :: C.OracleHTTPS -> Maybe (String, String)
getHTTPSCertificates (C.OracleHTTPS _ _ auth) = case auth of
                                                  (C.CertID (C.AuthenticationCertificate c k)) -> Just (c,k)
                                                  _ -> Nothing 

get :: H.Manager -> String -> IO String
get manager path = do

  request <- H.parseUrl path
  response <- H.httpLbs request manager

  return . show $ H.responseBody response


makeHTTPManager :: C.OracleHTTPS -> IO H.Manager
makeHTTPManager oracle = do
  certs <- makeCertificateStore <$> readSignedObject (C.oracleCert oracle)
  let supported = def { supportedVersions = [TLS12]
                      , supportedCiphers = ciphersuite_strong
                      , supportedHashSignatures = [ (HashSHA512, SignatureRSA) ] }
      shared = def { sharedCAStore = certs }
      hooks = def { onCertificateRequest = clientCertHook oracle }
      templateParams = defaultParamsClient "vanguard-node" ""
      clientParams = templateParams { clientSupported = supported
                                    , clientShared = shared 
                                    , clientHooks = hooks }
      tls = TLSSettings clientParams

  H.newManager $ mkManagerSettings tls Nothing
  
                                       
clientCertHook :: C.OracleHTTPS 
               -> ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName])
               -> IO (Maybe (CertificateChain, PrivKey))
clientCertHook _ _ = do 
  cert <- readSignedObject "test/mycert.cert"
  key <- readKeyFile "test/mykey.key"
  return $ Just (CertificateChain cert, head key)
