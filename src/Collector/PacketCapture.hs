module Collector.PacketCapture
( directTo ) where

import Network.Pcap
import qualified Data.ByteString as B

directTo :: String -> (B.ByteString -> IO ()) -> IO ()
directTo iface rc = do
  device <- openLive iface 0xFFFF False 0
  setFilter device "ip" True 0

  putStrLn $ "Listening on " ++ iface
  loopBS device (-1) (\_ bs -> rc bs)
  return ()
