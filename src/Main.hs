module Main where

import Control.Concurrent

import Collector.PacketCapture

import Network.TunTap
import System.Posix.IO
import ProcessThread

import Routing.Routing
import Routing.RoutingTable
import Routing.PacketParsing.Ether

import Relay.Relay
import Relay.Debug

import Command
import Environments

-- Only needed when setting up router
import Routing.PacketParsing.IP4 (addr, parseIP4)
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString as B
import Debug.QueueReader (makeQueueReader, newQueueAndReader)
-- End temporary section

main :: IO ()
main = do
  (routeChan, rt) <- makeRouter
  createRoutingTable rt

  q <- newQueueAndReader (\bs -> putStrLn.show $ parseIP4 bs)
  
  --etherStripper <- makeEtherStripper $ \bs -> bs `routeTo` q
  --"wlp3s0" `directTo` (\bs -> bs `routeTo` etherStripper)

  fd <- openTunTap TUN "vgl0" [noPI]
  fd `handleToAction` (\bs -> bs `routeTo` q)

  -- Start a command line
  let env = Environment rt
  commandLine `manageWith` env
  --closeFd fd
  return ()

-- Test function, building a basic routing table
createRoutingTable :: RoutingTable -> IO ()
createRoutingTable rt = do
  rt `setLocal` (addr "192.168.1.100")
  rt `setVirtual` (addr "10.0.0.1")
  inj <- getInjectionQueue rt
  makeQueueReader inj (\bs -> putStrLn $ show bs)
  return ()
