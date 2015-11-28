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
                      newConn <- try (newUDPconn rt) :: IO (Either SomeException (TQueue B.ByteString, UDPPair, Addr, Addr))
                      case newConn of
                        Left err -> putStrLn "New connection failed:" >> (putStrLn $ show err)
                        Right (q, udpp, !vad, !outad) -> newRoute rt vad (outad, q) >> putStrLn "New route added."
  | otherwise = putStrLn $ "Invalid command: " ++ cmd

newUDPconn :: RoutingTable -> IO (TQueue B.ByteString, UDPPair, Addr, Addr)
newUDPconn rt = do
  (out:op:inb:ip:vadd:_) <- sequence $ map prompt ["Outbound","Port","Inbound","Port","Register at"]
  inj <- getInjectionQueue rt
  let vad = stringToAddr vadd
      outad = stringToAddr out
  udpp <- relayPair (out,op) (inb,ip)
  outstream <- makeRelay udpp inj
  return $ (outstream, udpp, vad, outad)
  where
    prompt pr = do
      putStrLn $ pr ++":"
      getLine

    stringToAddr :: String -> Addr
    stringToAddr = read
