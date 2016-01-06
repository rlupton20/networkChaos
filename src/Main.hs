module Main where

import Control.Concurrent

import Network.TunTap
import Network.ReaderThread

import Routing.Routing
import Routing.RoutingTable
import Routing.PacketParsing.Ether

import Relay.Relay
import Relay.Debug

import ProcUnit
import Types
import Command
import Environments

import System.Environment

-- Only needed when setting up router
import Routing.PacketParsing.IP4 (parseIP4)
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString as B
-- End temporary section

main :: IO ()
main = do
  -- First lets deal with the command line arguments
  [device, myip] <- getArgs

  tun <- openTUN device
  
  injector <- procUnit (\bs -> do
                          putStrLn.show $ parseIP4 bs
                          writeTT tun bs )

  (router, rt) <- makeRouter injector
  
  myad <- addr "10.0.0.1"--myip
  rt `setAddr` myad
  
  onTT tun (\bs -> bs `passTo` router)

  -- Start a command line
  let env = Environment rt
  commandLine `manageWith` env
  closeTT tun
  return ()
