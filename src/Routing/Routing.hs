{-# LANGUAGE BangPatterns #-}
module Routing.Routing
( makeRouter
, routeTo ) where

import Routing.RoutingTable
import Routing.PacketParsing.IP4

import ProcUnit

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad

makeRouter :: ProcUnit B.ByteString () -> IO (ProcUnit B.ByteString (), RoutingTable)
makeRouter inj = do
  table <- newRoutingTable inj
  router <- procUnit (\x -> x `routeWith` table)
  return (router, table)

routeTo :: a -> (TQueue a) -> IO ()
routeTo x xq = atomically $ writeTQueue xq x

-- routeWith takes a TQueue of packets, and a RoutingTable,
-- and puts each packet on the correct exit queue
routeWith :: B.ByteString -> RoutingTable -> IO ()
routeWith bs rt = do
  let mppck = parseIP4 bs
  case mppck of
    Just ppck -> do
      redir <- lookup ppck
      case redir of
        Just rchan -> atomically $ writeTQueue rchan bs
        Nothing -> return ()
    Nothing -> return ()
  where
    lookup ppck = do
      let dest = getDest ppck
      redirect <- dest `getDirectionWith` rt
      return $ fmap snd redirect
