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
import           Data.X509                 ( DistinguishedName, CertificateChain )
import           Data.X509.File            ( readSignedObject, readKeyFile )
import           Data.X509.CertificateStore( makeCertificateStore )
import           Data.Default.Class        ( def ) 

import qualified Config as C


-- |An Oracle is an abstract data type which represents a central
-- source of truth for obtaining information about the network.
-- It is used to aid bootstrapping the network for instance.
data Oracle = Oracle { address :: String
                     , cert :: String } deriving (Show, Eq)

makeOracle :: C.OracleConfig -> IO Oracle
makeOracle config = return $ Oracle (C.address config) (C.oracleCert config)


get :: H.Manager -> String -> IO String
get manager path = do

  request <- H.parseUrl path
  response <- H.httpLbs request manager

  return . show $ H.responseBody response


makeHTTPManager :: Oracle -> IO H.Manager
makeHTTPManager oracle = do
  certs <- makeCertificateStore <$> readSignedObject (cert oracle)
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
  
                                       
clientCertHook :: Oracle 
               -> ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName])
               -> IO (Maybe (CertificateChain, PrivKey))
clientCertHook _ _ = return Nothing
