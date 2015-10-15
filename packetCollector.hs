import System.Environment

import Network.Pcap
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Net.Packet
import Net.PacketParsing
import qualified  Net.Ethernet
import qualified Net.IPv4

import qualified Data.ByteString as B

import Routing

formatForParsing :: B.ByteString -> InPacket
formatForParsing p = toInPack $ listArray (0, B.length p - 1) (B.unpack p)

parseIP4 :: B.ByteString -> Maybe (Net.Ethernet.Packet (Net.IPv4.Packet InPacket))
parseIP4 = doParse . formatForParsing

-- This checks that packets once parsed, unparse to the same thing.
-- Note how the interface to Net.Ethernet.Packet is incomplete.
unparseTest :: B.ByteString -> Maybe Bool
unparseTest pbs = do
                  let p = formatForParsing pbs
                  pck <- parseIP4 pbs
                  let pu = loopbackout p
                      pcku = Net.Ethernet.unparse $ fmap doUnparse pck
                  return (outBytes pu == outBytes pcku)

filterAndProcess :: PktHdr -> B.ByteString -> IO ()
filterAndProcess _ packet = do
  let peeled = parseIP4 packet
      ip4Packet = fmap Net.Ethernet.content peeled
  case ip4Packet of
    Nothing -> putStrLn "Bad Packet"
    (Just pack) -> processIP4 pack

main :: IO ()
main = do
  args <- getArgs
  let interface = if null args then "rvl0" else (args!!0)
  
  device <- openLive interface 0xFFFF False 0
  setFilter device "ether proto 0x0800" True 0

  putStrLn $ "Listening on " ++ interface
  loopBS device (-1) filterAndProcess
  return ()
