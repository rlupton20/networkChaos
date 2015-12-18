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

import System.Environment

-- Only needed when setting up router
import Routing.PacketParsing.IP4 (addr, parseIP4)
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString as B
import Debug.QueueReader (makeQueueReader, newQueueAndReader)
-- End temporary section

main :: IO ()
main = do
  -- First lets deal with the command line arguments
  [device, myip] <- getArgs
  
  (routeChan, rt) <- makeRouter
  createRoutingTable rt myip

  q <- newQueueAndReader (\bs -> putStrLn.show $ parseIP4 bs)

  tun <- openTUN device
  onTT tun (\bs -> bs `routeTo` routeChan)

  -- Start a command line
  let env = Environment rt
  commandLine `manageWith` env
  closeTT tun
  return ()

-- Test function, building a basic routing table
createRoutingTable :: RoutingTable -> String -> IO ()
createRoutingTable rt ip = do
  rt `setAddr` (addr ip)
  inj <- getInjectionQueue rt
  makeQueueReader inj (\bs -> putStrLn.show $ parseIP4 bs)
  return ()
