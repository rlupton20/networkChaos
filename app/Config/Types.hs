module Config.Types where

data VanguardConfig = VanguardConfig { net :: NetConfig
                                     , bootstrap :: Bootstrap } deriving (Eq, Show)

data Bootstrap = Bootstrap { address :: String
                           , node :: String } deriving (Eq, Show)

data NetConfig = NetConfig { device :: String
                           , ip :: String } deriving (Eq, Show)
