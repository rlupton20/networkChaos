module Config.Types where

import Control.Applicative

data VanguardConfig = VanguardConfig { net :: NetConfig
                                     , dht :: DistributedHashTable } deriving (Eq, Show)

data DistributedHashTable = DistributedHashTable { db :: String
                                                 , address :: String } deriving (Eq, Show)

data NetConfig = NetConfig { device :: String
                           , ip :: String } deriving (Eq, Show)
