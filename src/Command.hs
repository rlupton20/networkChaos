{-# LANGUAGE BangPatterns #-}
module Command
( commandLine ) where

import Environments
import Control.Monad.IO.Class
  
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
                      pp@((ins,_),(outs,_)) <- liftIO newUDPSockets
                      newConn <- tryM (newUDPconn pp) :: Manager (Either SomeException (TQueue B.ByteString, UDPPair, Addr, Addr))
                      liftIO $ case newConn of
                                    Left err -> putStrLn "New connection failed:" >> (putStrLn $ show err) >> close ins >> close outs
                                    Right (q, _, !vad, !outad) -> newRoute rt vad (outad, q) >> putStrLn "New route added."
  | otherwise = liftIO $ putStrLn $ "Invalid command: " ++ cmd

newUDPconn :: PrePair -> Manager (TQueue B.ByteString, UDPPair, Addr, Addr)
newUDPconn pp = do
  injIO <- asks ( getInjectionQueue . routingTable)  -- asks ... returns an IO action to get the injector queue
  liftIO $ do
    inj <- injIO
    (out:op:inb:ip:vadd:_) <- sequence $ fmap prompt ["Outbound","Port","Inbound","Port","Register at"]
    let vad = stringToAddr vadd
        outad = stringToAddr out
    udpp <- relayPair pp (out,op) (inb,ip)
    outstream <- makeRelay udpp inj
    return $ (outstream, udpp, vad, outad)
  where
    prompt pr = do
      putStrLn $ pr ++":"
      getLine

    stringToAddr :: String -> Addr
    stringToAddr = read
