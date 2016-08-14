{-# LANGUAGE OverloadedStrings #-}
module Config
( NetConfig(..)
, OracleConfig(..)
, VanguardConfig(..)
, loadConfigFromFile
) where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>),(<*>),empty)

import Config.Types

instance Y.FromJSON VanguardConfig where
  parseJSON (Y.Object v) = VanguardConfig <$> v .: "net" <*> v .: "oracle"
  parseJSON _ = empty

instance Y.FromJSON NetConfig where
  parseJSON (Y.Object v) = NetConfig <$> v .: "device" <*> v .: "address"
  parseJSON _ = empty

instance Y.FromJSON OracleConfig where
  parseJSON (Y.Object v) = OracleConfig <$> v .: "address" <*> v .: "oracleCert"
  parseJSON _ = empty

loadConfigFromFile :: String -> IO (Either Y.ParseException VanguardConfig)
loadConfigFromFile file = Y.decodeFileEither file
