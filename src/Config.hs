{-# LANGUAGE OverloadedStrings #-}
module Config where

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

loadConfig :: String -> IO (Either ParseException VanguardConfig)
loadConfig file = decodeFileEither file
