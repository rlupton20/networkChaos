{-# LANGUAGE OverloadedStrings #-}
module Config
( NetConfig(..)
, Bootstrap(..)
, VanguardConfig(..)
, loadConfigFromFile
) where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>),(<*>),empty)

import Config.Types

instance Y.FromJSON VanguardConfig where
  parseJSON (Y.Object v) = VanguardConfig <$> v .: "net" <*> v .: "bootstrap"
  parseJSON _ = empty

instance Y.FromJSON NetConfig where
  parseJSON (Y.Object v) = NetConfig <$> v .: "device" <*> v .: "address"
  parseJSON _ = empty

instance Y.FromJSON Bootstrap where
  parseJSON (Y.Object v) = Bootstrap <$> v .: "address" <*> v .: "node"
  parseJSON _ = empty

loadConfigFromFile :: String -> IO (Either Y.ParseException VanguardConfig)
loadConfigFromFile file = Y.decodeFileEither file
