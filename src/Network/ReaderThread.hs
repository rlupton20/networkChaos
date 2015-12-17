module Network.ReaderThread
( onTUN ) where

import qualified Data.ByteString as B
import Control.Concurrent
import Network.TunTap

onTUN :: TUNDevice -> (B.ByteString -> IO ()) -> IO ()
onTUN dev action = (forkIO $ loop dev action) >> return ()
  where loop dev action = do
          bs <- readTUN dev
          action bs
          loop dev action
