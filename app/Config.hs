{-# LANGUAGE OverloadedStrings #-}
module Config
( NetConfig(..)
, OracleConfig(..)
, VanguardConfig(..)
, AuthenticationCertificate(..)
, OracleAuth(..)
, loadConfigFromFile
) where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>),(<*>),empty)

import Config.Types


loadConfigFromFile :: String -> IO (Either Y.ParseException VanguardConfig)
loadConfigFromFile file = Y.decodeFileEither file


instance Y.FromJSON VanguardConfig where
  parseJSON (Y.Object v) = VanguardConfig <$> v .: "net" <*> v .: "oracle"
  parseJSON _ = empty

instance Y.FromJSON NetConfig where
  parseJSON (Y.Object v) = NetConfig <$> v .: "device" <*> v .: "address"
  parseJSON _ = empty

instance Y.FromJSON OracleConfig where
  parseJSON (Y.Object v) = OracleConfig <$> v .: "address" <*> v .: "oracle-certificate"
  parseJSON _ = empty

instance Y.FromJSON AuthenticationCertificate where
  parseJSON (Y.Object v) = AuthenticationCertificate <$> v .: "signed-certificate" <*> v .: "private-key"
  parseJSON _ = empty
  
instance Y.FromJSON OracleAuth where
  parseJSON (Y.Object v) = CertID <$> v .: "signed-certificate"
  parseJSON _ = empty
