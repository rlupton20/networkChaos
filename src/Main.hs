module Main where

import Control.Concurrent

import Network.TunTap
import Network.ReaderThread

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

device :: String
device = "vgl0"

main :: IO ()
main = do
  (routeChan, rt) <- makeRouter
  createRoutingTable rt

  q <- newQueueAndReader (\bs -> putStrLn.show $ parseIP4 bs)

  tun <- openTUN device
  onTUN tun (\bs -> bs `routeTo` q)

  -- Start a command line
  let env = Environment rt
  commandLine `manageWith` env
  closeTUN tun
  return ()

-- Test function, building a basic routing table
createRoutingTable :: RoutingTable -> IO ()
createRoutingTable rt = do
  rt `setLocal` (addr "192.168.1.100")
  rt `setVirtual` (addr "10.0.0.1")
  inj <- getInjectionQueue rt
  makeQueueReader inj (\bs -> putStrLn $ show bs)
  return ()
