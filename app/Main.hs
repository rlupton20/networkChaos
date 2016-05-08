module Main where

import Config

import Network.TunTap

import Control.Concurrent.Stack

import Routing.Routing
import Routing.RoutingTable

import Relay.Relay
--import Relay.Debug

import Types
import Utils

import Command
import Manager

import Command.Types
import Command.CliTypes
import Command.CommandLine

import System.Environment (getArgs)
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
  (injectQueue, injector) <- specInjector tt
  register $ injector

  (router, routeQueue, rt) <- specRouter injectQueue ip
  register $ router
  
  -- Now we build the packet reader
  let reader = forever $ do
        bs <- (readTT tt)
        bs `passTo`routeQueue
  register $ blocksInForeign reader

  -- Now we build the management system
  (manager, env) <- specManager rt
  register manager

  -- Lastly we start the command line
  let cq = commandQueue env
      post = postCommand cq
  register $ (runCli commandLine $ CliComm post)
  
  where

    specInjector :: TunTap -> Stack (PacketQueue, IO ())
    specInjector tt = liftIO $ do
      pq <- newQueue
      let injector = forever $ do
            bs <- readQueue pq
            putStrLn.show $ parseIP4 bs
            writeTT tt bs
      return (pq, injector)

    specRouter :: Injector -> String ->
                   Stack (IO (), PacketQueue, RoutingTable)
    specRouter injectQueue ip = liftIO $ do
      routingQueue <- newQueue
      (route, rt) <- makeRouter injectQueue
      -- Setup the routing table
      myad <- addr ip
      rt `setAddr` myad
      let router = forever $ readQueue routingQueue >>= route
      return (router, routingQueue, rt)

    specManager :: RoutingTable -> Stack (IO (), Environment)
    specManager rt = liftIO $ do
      env <- makeManaged rt
      return $ (commander `manage` env, env)
