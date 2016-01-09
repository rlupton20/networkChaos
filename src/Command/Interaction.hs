module Command.Interaction where

import Command.Types

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

direct :: CommandQueue -> IO ()
direct cq = do
  pn <- newEmptyTMVarIO
  ca <- newEmptyTMVarIO
  cp <- newEmptyTMVarIO
  va <- newEmptyTMVarIO

  postCommand cq $ DirectConnection pn ca cp va

  portNum <- atomically $ takeTMVar pn
  tell $ "Connection on port " ++ (show portNum)

  ask "IP address:" ca
  ask "Port Number:" cp
  ask "Register at:" va
  where
    
    tell = putStrLn

    ask str v = putStrLn str >>
                getLine >>= ( \str -> atomically $ putTMVar v str)
