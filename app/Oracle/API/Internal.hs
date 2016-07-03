module Oracle.API.Internal where


import Config as C


-- |An Oracle is an abstract data type which represents a central
-- source of truth for obtaining information about the network.
-- It is used to aid bootstrapping the network for instance.
data Oracle = Oracle { address :: String } deriving (Show, Eq)

makeOracle :: C.OracleConfig -> IO Oracle
makeOracle config = return $ Oracle (C.address config)
