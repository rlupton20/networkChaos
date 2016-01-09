module Main where

import Network.TunTap

import Routing.Routing
import Routing.RoutingTable

import Relay.Relay
import Relay.Debug

import Types
import Utils
import ProcUnit
import Command
import Environments

import Control.Concurrent
import System.Environment
import Control.Monad
import Control.Exception

-- Only needed when setting up router
import Debug.PacketParsing.IP4 (parseIP4)
-- End temporary section

main :: IO ()
main = do
  -- First lets deal with the command line arguments
  [device, myip] <- getArgs

  withTUN device $ \tun -> do
    injector <- procUnit (\bs -> do
                          putStrLn.show $ parseIP4 bs
                          writeTT tun bs )

    (router, rt) <- makeRouter injector
  
    myad <- addr myip
    rt `setAddr` myad

    forkFinally
      (forever $ do readTT tun >>= \bs -> bs `passTo` router)
      rethrowException

    -- Start a command line
    env <- makeEnvironmentWith rt
    commandLine `manageWith` env

  return ()
