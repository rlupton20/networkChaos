{-# LANGUAGE BangPatterns #-}
module Debug.PacketParsing.IP4
( parseIP4
, getSource
, setSource
, getDest
, setDest
, toBytes )
where

import Debug.PacketParsing.Parsing

import qualified Data.ByteString as B
import Net.Packet
import Net.PacketParsing
import Net.IPv4

parseIP4 :: B.ByteString -> Maybe (Net.IPv4.Packet InPacket)
parseIP4 = doParse . restruct

getSource :: Net.IPv4.Packet InPacket -> Addr
getSource = source

setSource :: Net.IPv4.Packet InPacket -> Addr -> Net.IPv4.Packet InPacket
setSource pck ad = let !rpck = pck { source = ad } in rpck

getDest :: Net.IPv4.Packet InPacket -> Addr
getDest = dest

setDest :: Net.IPv4.Packet InPacket -> Addr -> Net.IPv4.Packet InPacket
setDest pck ad = let !rpck = pck { dest = ad } in rpck

toBytes :: Net.IPv4.Packet InPacket -> B.ByteString
toBytes pck = let !bs = (B.pack . outBytes $ doUnparse pck) in bs

