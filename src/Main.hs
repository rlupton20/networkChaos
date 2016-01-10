module Main where

import Network.TunTap

import Control.Concurrent.Stack

import Routing.Routing
import Routing.RoutingTable

import Relay.Relay
import Relay.Debug

import Types
import Utils
import Control.IO.Builder

import Command
import Manager

import Command.Types
import Command.CliTypes
import Command.CommandLine

import System.Environment
import Control.Monad
import Control.Monad.IO.Class

-- Only needed when setting up router
import Debug.PacketParsing.IP4 (parseIP4)
-- End temporary section

main :: IO ()
main = do
  -- First lets deal with the command line arguments
  [device, myip] <- getArgs

  withTUN device $ \tun -> runStack (core tun myip)

core :: TunTap -> String -> Stack ()
core tt ip = do
  -- First lets build an injector worker
  injector <- liftIO $ newWorker $ \bs -> do
    putStrLn.show $ parseIP4 bs
    writeTT tt bs

  register $ build injector

  -- Build the router
  (router, rt) <- liftIO $ do
    (route, rt) <- makeRouter injector
    router <- newWorker route
    -- Setup the routing table
    myad <- addr ip
    rt `setAddr` myad
    return (router, rt)

  register $ build router
  
  -- makeWorkSourceOf
  -- Now we build the packet reader
  let reader = (readTT tt) `makeWorkSourceOf` router

  register $ blocksInForeign (build reader)

  -- Now we build the management system
  (manager, env) <- liftIO $ do
    env <- makeManaged rt
    return $ (commander `manageWith` env, env)

  register manager

  let cq = commandQueue env
      post = postCommand cq

  register $ (runCli commandLine $ CliComm post)
  
