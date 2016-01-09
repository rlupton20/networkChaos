{-# LANGUAGE BangPatterns #-}
module Command
( commander ) where

import Manager
import Types
import Utils
  
import Routing.RoutingTable
import Relay.Relay
import Relay.Interface

import Command.Types

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Monad.IO.Class (liftIO)
import Control.Exception

import Network.Socket

import qualified Data.ByteString as B

commander :: Manager ()
commander = do
  cq <- asks (commandQueue)
  cmd <- liftIO $ getCommand cq
  if cmd == Quit then return () else process cmd >> commander

-- Need to keep track of spawned threads, and make exception safe
process :: Command -> Manager ()
process dc@(DirectConnection _ _ _ _) = ask >>= \env -> liftIO $ forkIO ((direct dc) `manageWith` env) >> return ()
process _ = liftIO $ error "Invalid command reached processor."

direct :: Command -> Manager ()
direct (DirectConnection sp ca cp va) = do
  rt <- asks (routingTable)
  let inj = getInjector rt
  liftIO $ do
    bracket (newUDPSocket) (close.getSocket) $ \udpsock -> do
      
      pn <- socketPort.getSocket $ udpsock
      atomically.(putTMVar sp) $ fromIntegral pn
      [cas,cps,vas] <- sequence $ map (atomically.takeTMVar) [ca,cp,va]
      
      resolve <- try $ makeConn udpsock cas cps vas :: IO (Either SomeException (UDPConn, Addr, Addr))

      case resolve of
        Left _ -> putStrLn "Error with input."
        Right (udpc, cad, vad) -> do
          out <- newTQueueIO
          bracket (newRoute rt vad (cad, out))
                  (\_ -> delRouteFor rt vad)
                  (\_ -> makeRelay udpc inj out)
      return ()

  where
    makeConn :: UDPSock -> String -> String -> String -> IO (UDPConn, Addr, Addr)
    makeConn udp cas cps vas = do
      ca <- addr cas
      va <- addr vas
      udpconn <- sockToConn udp (cas,cps)
      return (udpconn, ca, va)
      
      
{-
newUDPconn :: UDPSock -> Manager (TQueue B.ByteString, Addr, Addr)
newUDPconn pp = do
  injector <- asks ( getInjector . routingTable )
  liftIO $ do

    udpp <- sockToConn pp (cor,corP)
    outstream <- makeRelay udpp injector
-}
