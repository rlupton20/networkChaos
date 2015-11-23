module PacketParsing.Ether
( stripEther
, makeEtherStripper ) where

import Net.Packet
import Net.PacketParsing
import Net.Ethernet
import Net.IPv4

import qualified Data.ByteString as B

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

stripEther :: B.ByteString -> Maybe B.ByteString
stripEther = fmap (B.pack . outBytes . doUnparse) . tryRemoveEther

makeEtherStripper :: (B.ByteString -> IO ()) -> IO (TQueue B.ByteString)
makeEtherStripper outfeed = do
  infeed <- newTQueueIO
  tid <- forkIO $ loop infeed
  putStrLn $ "Ethernet header stripper started: " ++ show tid
  return infeed
  where
    loop infeed = do
      pck <- atomically $ readTQueue infeed
      case stripEther pck of
        (Just spck) -> outfeed spck
        Nothing -> return ()
      loop infeed
  
restruct :: B.ByteString -> InPacket
restruct pk = toInPack $ listArray (0, B.length pk - 1) (B.unpack pk)

tryRemoveEther :: B.ByteString -> Maybe (Net.IPv4.Packet InPacket)
tryRemoveEther = fmap (Net.Ethernet.content) . doParse . restruct

