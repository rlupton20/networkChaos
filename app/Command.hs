{-# LANGUAGE BangPatterns #-}
module Command
( commander ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket, try, SomeException)

import qualified Network.Socket as S

import Manager
import Types
import Utils
  
import Routing.RoutingTable
import Relay.Relay
import Relay.Interface

import Command.Types

-- |commander is a Manager process which takes commands and
-- spawns submanagers to execute those commands. It uses a
-- subordinate function process to decide what Manager threads
-- to spawn. The only commands it deals with explictly is quitting.
commander :: Manager ()
commander = do
  cq <- fromEnvironment (commandQueue)
  cmd <- liftIO $ getCommand cq
  if cmd == Quit then return () else process cmd >> commander

-- |process takes a command, and forks an appropriate submanager
-- to deal with it.
process :: Command -> Manager ()
process dc@(DirectConnection _ _ _ _) = spawn (direct dc) >> return ()
process _ = liftIO $ error "Invalid command reached processor."

-- |direct sets up a direct, explicitly specified UDP connection
-- with another endpoint. It opens a socket, and sends the source
-- of commands information about the port, then waits for information
-- on the endpoint, which is uses to program a route into the routing
-- table, and fork some threads for handling communication. This is
-- mostly used for testing and will probably be removed eventually.
direct :: Command -> Manager ()
direct (DirectConnection lp' ad' po' vt') = do
  table <- fromEnvironment (routingTable)
  let injector = getInjector table
  liftIO $ do
    bracket (newUDPSocket) (S.close . getSocket) $ \udpsocket -> do
      localport <- S.socketPort . getSocket $ udpsocket
      atomically.(putTMVar lp') $ fromIntegral localport
      [address',port',virtual'] <- sequence $
        map (atomically.takeTMVar) [ad',po',vt']
      
      resolve <- try $ connect udpsocket address' port' virtual' :: IO (Either SomeException (UDPConnection, Addr, Addr))

      case resolve of
        Left _ -> putStrLn "Error with input."
        Right (connection, address, virtual) -> do
          out <- newQueue
          let outgoing = (address, out)
          table `withRoute` (virtual #-> outgoing) $
            makeRelay connection injector out
      return ()

  where
    connect :: UDPSocket -> String -> String -> String -> IO (UDPConnection, Addr, Addr)
    connect udpsocket address' port' virtual' = do
      address <- addr address'
      virtual <- addr virtual'
      connection <- sockToConn udpsocket (address',port')
      return (connection, address, virtual)
