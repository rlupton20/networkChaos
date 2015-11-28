module Relay.Debug
( StdIO(StdIO)
, OutOnly
, outRelayWith ) where

import Relay.Connection

import Control.Concurrent

import qualified Data.ByteString.Char8 as B

-- StdIO is a debugging connection, which simply makes use of the
-- standard input and output
data StdIO = StdIO deriving (Show)

instance Connection StdIO where
  receiveOn _ = B.getLine
  sendOn str _ = B.putStrLn $ str
  closeConn _ = return ()

data OutOnly = OO (B.ByteString -> IO ())

instance Connection OutOnly where
  receiveOn _ = loop  -- Never return anything
    where
      loop = threadDelay 1000000 >> loop
  sendOn str (OO action) = action str
  closeConn _ = return ()
  
outRelayWith :: (B.ByteString -> IO ()) -> OutOnly
outRelayWith = OO
