module Command.Interaction where

import Command.CliTypes
import Command.Types

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

direct :: CLI ()
direct = do
  post <- asks (sendCommand)
  liftIO $ do
    pn <- newEmptyTMVarIO
    ca <- newEmptyTMVarIO
    cp <- newEmptyTMVarIO
    va <- newEmptyTMVarIO

    post $ DirectConnection pn ca cp va

    portNum <- atomically $ takeTMVar pn
    tell $ "Connection on port " ++ (show portNum)

    ask "IP address:" ca
    ask "Port Number:" cp
    ask "Register at:" va
  where
    
    tell = putStrLn

    ask str v = putStrLn str >>
                getLine >>= ( \str -> atomically $ putTMVar v str)
