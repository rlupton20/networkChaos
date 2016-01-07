{-# LANGUAGE BangPatterns #-}
module Command
( commandLine ) where

import Environments
import Types
  
import Routing.RoutingTable
import Relay.Relay
import Relay.Interface

import Control.Concurrent.STM.TQueue
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)

import Network.Socket

import qualified Data.ByteString as B

commandLine :: Manager ()
commandLine = do
  cmd <- liftIO $ getLine
  if cmd == "quit" then return () else process cmd >> commandLine

process :: String -> Manager ()
process cmd
  | cmd == "direct" = direct
  | otherwise = liftIO $ putStrLn $ "Invalid command: " ++ cmd
  where

    -- direct creates a new direct (unencrypted)
    -- connection between two addresses.
    direct :: Manager ()
    direct = do
      rt <- asks routingTable
      maskManager $ \restore -> do
        pp <- liftIO newUDPSocket
        
        let sock = getSocket pp
            sockaddr = getSockAddr pp

        -- Report the address of the new socket
        liftIO $ putStrLn $ "New socket on: " ++ show sockaddr
      
        newConn <- tryManager (restore $ newUDPconn pp) :: Manager (Either SomeException (TQueue B.ByteString, Addr, Addr))
      
        liftIO $ case newConn of
          Left err -> do
            putStrLn "New direct connection failed:"
            putStrLn $ show err
            close sock
          Right (q, !vad, !corad) -> do
            newRoute rt vad (corad, q)
            putStrLn "New route added."
        

newUDPconn :: UDPSock -> Manager (TQueue B.ByteString, Addr, Addr)
newUDPconn pp = do
  injector <- asks ( getInjector . routingTable )
  liftIO $ do
    [cor,corP,vadd] <- sequence $ fmap prompt ["Correspondance IP","Port","Register at"]

    -- Convert input strings into Addrs
    corad <- addr cor
    vad <- addr vadd

    udpp <- sockToConn pp (cor,corP)
    outstream <- makeRelay udpp injector
    return $ (outstream, vad, corad)
  where
    prompt :: String -> IO String
    prompt pr = do
      putStrLn $ pr ++" >>"
      getLine
