{-# LANGUAGE BangPatterns #-}
module Command
( commandLine ) where

import Environments
import Control.Monad.IO.Class
import Utils
  
import Routing.RoutingTable
import Relay.Relay
import Relay.Interface

import Control.Concurrent.STM.TQueue

import Control.Exception

import Network.Socket
import Net.IPv4 (Addr)

import qualified Data.ByteString as B

commandLine :: Manager ()
commandLine = do
  cmd <- liftIO $ getLine
  if cmd == "quit" then return () else process cmd >> commandLine

process :: String -> Manager ()
process cmd
  | cmd == "direct" = do
                      rt <- asks routingTable
                      pp <- liftIO newUDPSocket
                      let sock = getSocket pp
                      newConn <- tryM (newUDPconn pp) :: Manager (Either SomeException (TQueue B.ByteString, UDPConn, Addr, Addr))
                      liftIO $ case newConn of
                                    Left err -> putStrLn "New connection failed:" >> (putStrLn $ show err) >> close sock
                                    Right (q, _, !vad, !corad) -> newRoute rt vad (corad, q) >> putStrLn "New route added."
  | otherwise = liftIO $ putStrLn $ "Invalid command: " ++ cmd

newUDPconn :: UDPSock -> Manager (TQueue B.ByteString, UDPConn, Addr, Addr)
newUDPconn pp = do
  injIO <- asks ( getInjectionQueue . routingTable )  -- asks ... returns an IO action to get the injector queue
  liftIO $ do
    inj <- injIO
    [cor,corP,vadd] <- sequence $ fmap prompt ["Correspondance IP:","Port","Register at"]

    corad <- (readM cor :: IO Addr)
    vad <- (readM vadd :: IO Addr)

    udpp <- sockToConn pp (cor,corP)
    outstream <- makeRelay udpp inj
    return $ (outstream, udpp, vad, corad)
  where
    prompt :: String -> IO String
    prompt pr = do
      putStrLn $ pr ++":"
      getLine
