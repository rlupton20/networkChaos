module Network.ReaderThread
( fromTUN ) where

import qualified Data.ByteString as B
import Control.Concurrent
import Network.TunTap

fromTUN :: TUNDevice -> (B.ByteString -> IO ()) -> IO ()
fromTUN dev action = (forkIO $ loop dev action) >> return ()
  where loop dev action = do
          bs <- readTUN dev
          action bs
          loop dev action
