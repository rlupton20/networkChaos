module Command.Interaction
( direct ) where

import Command.CliTypes
import Command.Types

import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVarIO
                                    , takeTMVar, putTMVar)

-- |direct is a command line operation which manages command
-- line interaction to establish a direct connection. It is
-- an endpoint for the direct Manager process. Communication is
-- via TMVars which are taken and put in a specific order. direct
-- provides the data structure (DirectConnection) with the TMVars to
-- contain the data, and they are passed back and forth as required.
direct :: CLI ()
direct = do
  post <- asks (sendCommand)
  liftIO $ do
    localport <- newEmptyTMVarIO
    address <- newEmptyTMVarIO
    port <- newEmptyTMVarIO
    virtual <- newEmptyTMVarIO

    post $ DirectConnection localport address port virtual

    portNum <- atomically $ takeTMVar localport
    tell $ "Connection on port " ++ (show portNum)

    ask "IP address:" address
    ask "Port Number:" port
    ask "Register at:" virtual
  where
    tell :: String -> IO ()
    tell = putStrLn

    ask :: String -> (TMVar String) -> IO ()
    ask str v = putStrLn str >>
                getLine >>= ( \str -> atomically $ putTMVar v str )
