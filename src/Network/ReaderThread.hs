module Network.ReaderThread
( onTT ) where

import qualified Data.ByteString as B
import Control.Concurrent
import Network.TunTap

onTT :: TunTap -> (B.ByteString -> IO ()) -> IO ()
onTT dev action = (forkIO $ loop dev action) >> return ()
  where loop dev action = do
          bs <- readTT dev
          action bs
          loop dev action
