{-# LANGUAGE BangPatterns #-}
module Command
( commandLine ) where

import Routing.RoutingTable
import Relay.Relay
import Relay.Interface

import Control.Concurrent.STM.TQueue

import Control.Exception

import Network.Socket
import Net.IPv4 (Addr)

import qualified Data.ByteString as B

commandLine :: RoutingTable -> IO ()
commandLine rt = do
  cmd <- getLine
  if cmd == "quit" then return () else process rt cmd >> commandLine rt

process :: RoutingTable -> String -> IO ()
process rt cmd
  | cmd == "direct" = do
                      pp@((ins,_),(outs,_)) <- newUDPSockets
                      newConn <- try (newUDPconn rt pp) :: IO (Either SomeException (TQueue B.ByteString, UDPPair, Addr, Addr))
                      case newConn of
                        Left err -> putStrLn "New connection failed:" >> (putStrLn $ show err) >> close ins >> close outs
                        Right (q, _, !vad, !outad) -> newRoute rt vad (outad, q) >> putStrLn "New route added."
  | otherwise = putStrLn $ "Invalid command: " ++ cmd

newUDPconn :: RoutingTable -> PrePair -> IO (TQueue B.ByteString, UDPPair, Addr, Addr)
newUDPconn rt pp = do
  (out:op:inb:ip:vadd:_) <- sequence $ fmap prompt ["Outbound","Port","Inbound","Port","Register at"]
  inj <- getInjectionQueue rt
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
