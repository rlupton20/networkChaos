module Config.Types where

data VanguardConfig = VanguardConfig { net :: NetConfig
                                     , oracle :: OracleConfig } deriving (Eq, Show)


data NetConfig = NetConfig { device :: String
                           , ip :: String } deriving (Eq, Show)


data OracleConfig = OracleConfig { address :: String
                                 , oracleCert :: String } deriving (Eq, Show)

data OracleAuth = CertID AuthenticationCertificate deriving (Eq, Show)


data AuthenticationCertificate = AuthenticationCertificate { cert :: String
                                                           , key :: String } deriving (Eq, Show)
