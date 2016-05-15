{-# LANGUAGE OverloadedStrings #-}
module Config
( NetConfig(..)
, DistributedHashTable(..)
, VanguardConfig(..)
, loadConfigFromFile
) where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>),(<*>),empty)

import Config.Types

instance Y.FromJSON VanguardConfig where
  parseJSON (Y.Object v) = VanguardConfig <$> v .: "net" <*> v .: "dht"
  parseJSON _ = empty

instance Y.FromJSON NetConfig where
  parseJSON (Y.Object v) = NetConfig <$> v .: "device" <*> v .: "address"
  parseJSON _ = empty

instance Y.FromJSON DistributedHashTable where
  parseJSON (Y.Object v) = DistributedHashTable <$> v .: "db" <*> v .: "address"
  parseJSON _ = empty

loadConfigFromFile :: String -> IO (Either Y.ParseException VanguardConfig)
loadConfigFromFile file = Y.decodeFileEither file
