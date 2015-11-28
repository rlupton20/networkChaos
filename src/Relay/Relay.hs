module Relay.Relay
( makeRelay ) where

import Relay.Connection
  
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString as B

makeRelay :: (Connection a) =>  a -> TQueue B.ByteString -> IO (TQueue B.ByteString)
makeRelay con outstream = do
  instream <- newTQueueIO
  return instream

