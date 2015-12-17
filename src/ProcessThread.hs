{-# LANGUAGE PackageImports #-}
module ProcessThread
( handleToAction ) where

import qualified Data.ByteString as B
import System.Posix.Types
import "unix-bytestring" System.Posix.IO.ByteString
import Control.Concurrent
import Network.TunTap

handleToAction :: Fd -> (B.ByteString -> IO ()) -> IO ()
handleToAction fd action = (forkIO $ loop fd action) >> return ()
  where loop fd action = do
          bs <- fdRead fd 4096
          action bs
          loop fd action
