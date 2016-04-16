module Main where

import Config

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
  -- Our command line argument is a configuration
  -- file for the network we want to connect to.
  [configFile] <- getArgs
  config <- loadConfigFromFile configFile

  case config of
    Left err -> error $ show err
    Right cfg -> do
      let dev = device $ net cfg
          myip = ip $ net cfg
      withTUN dev $ \tun -> runStack (core tun myip)
  


core :: TunTap -> String -> Stack ()
core tt ip = do
  injector <- specInjector tt
  register $ build injector

  (router, rt) <- specRouter injector ip
  register $ build router
  
  -- Now we build the packet reader
  let reader = (readTT tt) `makeWorkSourceOf` router
  register $ blocksInForeign (build reader)

  -- Now we build the management system
  (manager, env) <- specManager rt
  register manager

  -- Lastly we start the command line
  let cq = commandQueue env
      post = postCommand cq
  register $ (runCli commandLine $ CliComm post)
  
  where

    specInjector :: TunTap -> Stack Injector
    specInjector tt = liftIO $ newWorker $ \bs -> do
      putStrLn.show $ parseIP4 bs
      writeTT tt bs

    specRouter :: Injector -> String ->
                   Stack (Worker Packet, RoutingTable)
    specRouter injector ip = liftIO $ do
      (route, rt) <- makeRouter injector
      router <- newWorker route
      -- Setup the routing table
      myad <- addr ip
      rt `setAddr` myad
      return (router, rt)

    specManager :: RoutingTable -> Stack (IO (), Environment)
    specManager rt = liftIO $ do
      env <- makeManaged rt
      return $ (commander `manage` env, env)
