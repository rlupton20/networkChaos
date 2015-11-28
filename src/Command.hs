{-# LANGUAGE BangPatterns #-}
module Command
( commandLine ) where

import Routing.RoutingTable
import Relay.Relay
import Relay.Interface

import Control.Concurrent.STM.TQueue

import Control.Exception
import Net.IPv4 (Addr)

import qualified Data.ByteString as B

commandLine :: RoutingTable -> IO ()
commandLine rt = do
  cmd <- getLine
  if cmd == "quit" then return () else process rt cmd >> commandLine rt

process :: RoutingTable -> String -> IO ()
process rt cmd
  | cmd == "direct" = do
                      enc <- try (newUDPconn rt) :: IO (Either SomeException (TQueue B.ByteString, UDPPair, Addr, Addr))
                      case enc of
                        Left err -> do
                          putStrLn "New connection failed:"
                          putStrLn $ show err
                        Right (q, udpp, !vad, !outad) -> do
                                   putStrLn "Success"
                                   newRoute rt vad (outad, q)
  | otherwise = putStrLn $ "Invalid command: " ++ cmd

newUDPconn :: RoutingTable -> IO (TQueue B.ByteString, UDPPair, Addr, Addr)
newUDPconn rt = do
  putStrLn "Outbound:"
  out <- getLine
  putStrLn "Port:"
  op <- getLine
  putStrLn "Inbound:"
  inb <- getLine
  putStrLn "Port:"
  ip <- getLine
  putStrLn "Register at:"
  vadd <- getLine
  inj <- getInjectionQueue rt
  let vad = read vadd :: Addr
  let outad = read out :: Addr
  udpp <- relayPair (out,op) (inb,ip)
  outstream <- makeRelay udpp inj
  return $ (outstream, udpp, vad, outad)
  
