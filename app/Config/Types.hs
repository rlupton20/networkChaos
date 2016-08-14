module Config.Types where

data VanguardConfig = VanguardConfig { net :: NetConfig
                                     , oracle :: OracleConfig } deriving (Eq, Show)

data OracleConfig = OracleConfig { address :: String
                                 , oracleCert :: String } deriving (Eq, Show)

data NetConfig = NetConfig { device :: String
                           , ip :: String } deriving (Eq, Show)
