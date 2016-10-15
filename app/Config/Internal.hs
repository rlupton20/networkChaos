{-# LANGUAGE OverloadedStrings #-}
module Config.Internal where

import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Control.Applicative ((<$>),(<*>),empty)


loadConfigFromFile :: String -> IO (Either Y.ParseException VanguardConfig)
loadConfigFromFile file = Y.decodeFileEither file
  
                                             
data VanguardConfig = VanguardConfig { net :: NetConfig
                                     , control :: ControlConfig } deriving (Eq, Show)


data NetConfig = NetConfig { device :: String
                           , ip :: String } deriving (Eq, Show)


data ControlConfig = ControlConfig { socket :: String } deriving (Eq, Show)


instance Y.FromJSON VanguardConfig where
  parseJSON (Y.Object v) = VanguardConfig <$> v .: "net" <*> v .: "control"
  parseJSON _ = empty

  
instance Y.FromJSON NetConfig where
  parseJSON (Y.Object v) = NetConfig <$> v .: "device" <*> v .: "address"
  parseJSON _ = empty

  
instance Y.FromJSON ControlConfig where
  parseJSON (Y.Object v) = ControlConfig <$> v .: "socket"
  parseJSON _ = empty
