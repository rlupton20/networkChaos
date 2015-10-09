import Network.Pcap
import Net.Packet
import Net.PacketParsing
import qualified  Net.Ethernet
import qualified Net.IPv4
import qualified Data.ByteString as B

-- Any network interface can be put in here. rvl0 is a dummy network
-- we create using ip link in a script. Other interfaces can be used to
-- check functionality.
interface :: String
--interface = "rvl0"
interface = "enp0s18f2u1"

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

main :: IO ()
main = do
  device <- openLive interface 0xFFFF False 0
  setFilter device "ether proto 0x0800" True 0
  
  loopBS device (-1) (\_ packet -> putStrLn.show $ unparseTest packet)
  return ()
