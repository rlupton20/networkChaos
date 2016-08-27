{-# LANGUAGE OverloadedStrings #-}
module Config.Internal where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>),(<*>),empty)


loadConfigFromFile :: String -> IO (Either Y.ParseException VanguardConfig)
loadConfigFromFile file = Y.decodeFileEither file
  
                                             
data VanguardConfig = VanguardConfig { net :: NetConfig
                                     , oracle :: OracleHTTPS } deriving (Eq, Show)


data NetConfig = NetConfig { device :: String
                           , ip :: String } deriving (Eq, Show)


data OracleHTTPS = OracleHTTPS { address :: String
                               , oracleCert :: String 
                               , auth :: OracleHTTPSAuth } deriving (Eq, Show)

data OracleHTTPSAuth = CertID AuthenticationCertificate deriving (Eq, Show)


data AuthenticationCertificate = AuthenticationCertificate { cert :: String
                                                           , key :: String } deriving (Eq, Show)

instance Y.FromJSON VanguardConfig where
  parseJSON (Y.Object v) = VanguardConfig <$> v .: "net" <*> v .: "oracle"
  parseJSON _ = empty

instance Y.FromJSON NetConfig where
  parseJSON (Y.Object v) = NetConfig <$> v .: "device" <*> v .: "address"
  parseJSON _ = empty

instance Y.FromJSON OracleHTTPS where
  parseJSON (Y.Object v) = OracleHTTPS <$> v .: "address" <*> v .: "oracle-certificate" <*> v .: "authentication"
  parseJSON _ = empty

instance Y.FromJSON AuthenticationCertificate where
  parseJSON (Y.Object v) = AuthenticationCertificate <$> v .: "signed-certificate" <*> v .: "private-key"
  parseJSON _ = empty
  
instance Y.FromJSON OracleHTTPSAuth where
  parseJSON (Y.Object v) = CertID <$> v .: "signed-certificate"
  parseJSON _ = empty
