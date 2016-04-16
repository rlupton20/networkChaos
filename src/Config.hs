{-# LANGUAGE OverloadedStrings #-}
module Config
( NetConfig(..)
, DistributedHashTable(..)
, VanguardConfig(..)
, loadConfigFromFile
) where

import Config.Types

import Data.Yaml
import Control.Applicative

instance FromJSON VanguardConfig where
  parseJSON (Object v) = VanguardConfig <$> v .: "net" <*> v .: "dht"
  parseJSON _ = empty

instance FromJSON NetConfig where
  parseJSON (Object v) = NetConfig <$> v .: "device" <*> v .: "address"
  parseJSON _ = empty

instance FromJSON DistributedHashTable where
  parseJSON (Object v) = DistributedHashTable <$> v .: "db" <*> v .: "address"
  parseJSON _ = empty

loadConfigFromFile :: String -> IO (Either ParseException VanguardConfig)
loadConfigFromFile file = decodeFileEither file
