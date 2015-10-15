module Routing where

import System.Environment

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Net.Packet
import Net.PacketParsing
import qualified  Net.Ethernet
import qualified Net.IPv4

import qualified Data.ByteString as B

routing :: (Net.IPv4.Addr, Net.IPv4.Addr)
routing = (Net.IPv4.Addr 10 0 0 10, Net.IPv4.Addr 192 168 0 104)

addrToString :: Net.IPv4.Addr -> String
addrToString (Net.IPv4.Addr s1 s2 s3 s4) = init.concat.map (++".") $ map show [s1, s2, s3, s4]

processIP4 :: Net.IPv4.Packet InPacket -> IO ()
processIP4 packet = withSocketsDo $ do
  
  let destination = Net.IPv4.dest packet
      newdest = if (destination == fst routing) then snd routing else snd routing
      routedPacket = packet { Net.IPv4.dest = newdest }
      toSend = B.pack.outBytes $ doUnparse routedPacket

  putStrLn $ (show destination) ++ " --> " ++ (show newdest)

  ad <- getAddrInfo Nothing (Just $ addrToString newdest) Nothing
  let addr = addrAddress.head $ ad

  socket <- socket AF_INET Raw 255  
  connect socket addr
  sent <- send socket toSend
  
  putStrLn $ show sent ++ " bytes sent"

  return ()
