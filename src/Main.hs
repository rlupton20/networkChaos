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
import Manager

import Command.Types
import Command.CliTypes
import Command.CommandLine

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
    -- The following doesn't receive exceptions etc.
    injector <- procUnit (\bs -> do
                          putStrLn.show $ parseIP4 bs
                          writeTT tun bs )

    -- The router ProcUnit is also not safe under exceptions
    (router, rt) <- makeRouter injector
  
    myad <- addr myip
    rt `setAddr` myad

    -- The following needs to be made to receive exceptions
    -- from the main thread.
    forkIO
      (forever $ do readTT tun >>= \bs -> bs `passTo` router)

    -- Start a command line
    env <- makeManaged rt
    forkIO $ commander `manageWith` env

    let cq = commandQueue env
        post = postCommand cq
    runCli commandLine $ CliComm post

  return ()
