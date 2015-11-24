{-# LANGUAGE BangPatterns #-}
module Routing.PacketParsing.Ether
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

import Routing.PacketParsing.Parsing

-- The ethernet header is 14 bytes long so just drop it to get contents
-- WARNING: Different header types may cause problems, so if this
-- is kept later, it may need modifying. One advantage is its content
-- agnostic, so will deal with anything coming via ethernet.
-- See: tryRemoveEther, below.
stripEther :: B.ByteString -> B.ByteString
stripEther = B.drop 14

-- This is the old version of stripEther, which actually
-- parses the packet and extracts its contents.
stripEther' :: B.ByteString -> Maybe B.ByteString
stripEther' = fmap (B.pack . outBytes . doUnparse) . tryRemoveEther

makeEtherStripper :: (B.ByteString -> IO ()) -> IO (TQueue B.ByteString)
makeEtherStripper outfeed = do
  infeed <- newTQueueIO
  tid <- forkIO $ loop infeed
  putStrLn $ "Ethernet header stripper started: " ++ show tid
  return infeed
  where
    loop infeed = do
      pck <- atomically $ readTQueue infeed
      let !spck = stripEther pck
      outfeed spck
      loop infeed

-- This function (tries to) parse an ethernet packet, and
-- returns an internal IP packet if there is one.
-- Question: could this miss some packets? e.g. if not IP?
tryRemoveEther :: B.ByteString -> Maybe (Net.IPv4.Packet InPacket)
tryRemoveEther = fmap (Net.Ethernet.content) . doParse . restruct
