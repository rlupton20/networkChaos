module Debug.PacketParsing.Parsing
( restruct ) where

import Net.Packet (InPacket, toInPack, listArray)

import qualified Data.ByteString as B

restruct :: B.ByteString -> InPacket
restruct pk = toInPack $ listArray (0, B.length pk - 1) (B.unpack pk)
