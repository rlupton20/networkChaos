module Routing where

import System.Environment
import Control.Monad.IO.Class

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Net.Packet
import Net.PacketParsing
import qualified  Net.Ethernet
import qualified Net.IPv4

import qualified Data.ByteString as B
import Connection

routing :: (Net.IPv4.Addr, Net.IPv4.Addr)
routing = (Net.IPv4.Addr 10 0 0 10, Net.IPv4.Addr 127 0 0 1) --192 168 0 104)

addrToString :: Net.IPv4.Addr -> String
addrToString (Net.IPv4.Addr s1 s2 s3 s4) = init.concat.map (++".") $ map show [s1, s2, s3, s4]

processIP4 :: Net.IPv4.Packet InPacket -> IO ()
processIP4 packet = withSocketsDo $ do
  
  let destination = Net.IPv4.dest packet
      newdest = if (destination == fst routing) then snd routing else snd routing
      routedPacket = packet { Net.IPv4.dest = newdest }
      toSend = B.pack.outBytes $ doUnparse routedPacket

  putStrLn $ (show destination) ++ " --> " ++ (show $ Net.IPv4.dest routedPacket)

  --sent <- toSend `dispatchTo` (addrToString newdest)

  comms <- synchroSock $ addrToString newdest
  toSend `dispatchTo` comms
  putStrLn $ "Dispatched."

  return ()

-- This creates a normal one way socket Connection to the
-- provided address.
synchroSock :: (MonadIO m) => String -> m DualSockets
synchroSock addr = liftIO $ do
  (ad:_) <- getAddrInfo Nothing (Just addr) Nothing
  let sockAddr = addrAddress ad
  sock <- socket AF_INET Raw 255
  connect sock sockAddr
  putStrLn $ "Connected on " ++ show sockAddr
  return $ (DualSockets sock sock)
  
  
