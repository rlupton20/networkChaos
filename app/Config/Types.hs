module Config.Types where

data VanguardConfig = VanguardConfig { net :: NetConfig
                                     , oracle :: OracleHTTPS } deriving (Eq, Show)


data NetConfig = NetConfig { device :: String
                           , ip :: String } deriving (Eq, Show)


data OracleHTTPS = OracleHTTPS { address :: String
                               , oracleCert :: String 
                               , auth :: OracleHTTPSAuth } deriving (Eq, Show)

data OracleHTTPSAuth = CertID AuthenticationCertificate deriving (Eq, Show)


data AuthenticationCertificate = AuthenticationCertificate { cert :: String
                                                           , key :: String } deriving (Eq, Show)
