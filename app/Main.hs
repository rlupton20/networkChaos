module Main where

import System.Environment (getArgs)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

import Config

import Network.TunTap

import Control.Concurrent.Stack

import Routing.Routing
import Routing.RoutingTable
import Relay.Relay

import Manager

import Command
import Command.Types
import Command.CliTypes
import Command.CommandLine

import Types
import Utils

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
  

-- |core is a specification of the critical spine of the program.
-- The core threads are specified here, and run using runStack
-- in main.
core :: TunTap -> String -> Stack ()
core tt ip = do
  (injectQueue, injector) <- specInjector tt
  register injector

  (router, routeQueue, table) <- specRouter injectQueue ip
  register router
  
  -- Now we build the packet reader
  let reader = forever $ do
        bs <- readTT tt
        bs `passTo` routeQueue
  register $ blocksInForeign reader

  -- Now we build the management system
  (manager, env) <- specManager table
  register manager

  -- Lastly we start the command line
  let cq = commandQueue env
      post = postCommand cq
  register $ runCli commandLine (CliComm post)

  where

    specInjector :: TunTap -> Stack (PacketQueue, IO ())
    specInjector tt = liftIO $ do
      pq <- newQueue
      let injector = forever $ do
            bs <- readQueue pq
            putStrLn.show $ parseIP4 bs -- DEBUGGING
            writeTT tt bs
      return (pq, injector)

    specRouter :: Injector -> String ->
                   Stack (IO (), PacketQueue, RoutingTable)
    specRouter injectQueue ip = liftIO $ do
      routingQueue <- newQueue
      (route, table) <- makeRouter injectQueue
      -- Setup the routing table
      ad <- addr ip
      table `setAddr` ad
      let router = forever $ readQueue routingQueue >>= route
      return (router, routingQueue, table)

    specManager :: RoutingTable -> Stack (IO (), Environment)
    specManager table = liftIO $ do
      env <- makeManaged table
      return (commander `manage` env, env)
