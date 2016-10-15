module Command.Control
( controller ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (bracket, bracket_)
import Network.Socket ( Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix)
                      , Socket, defaultProtocol, socket, bind, close )
import System.Posix.Files ( removeLink )

import Command.ControlTypes (Control, ControlEnvironment)
  

controller :: String -> ControlEnvironment -> IO ()
controller socketPath _ = do
  liftIO $ withControlSocket socketPath $ \_ -> 
    let loop = do 
            cmd <- getLine
            if cmd == "quit" then return () else process cmd >> loop
    in loop


process :: String -> IO ()
process cmd
  | otherwise = putStrLn $ "Invalid command: " ++ cmd

  
-- |withUnixSocket opens a new (streaming) unix socket, with the promise
-- that it will be closed in the event of an exception, or when the passed
-- action is finished.
withUnixSocket :: (Socket -> IO a) -> IO a
withUnixSocket action = bracket
  (socket AF_UNIX Stream defaultProtocol)
  (\sock -> close sock) 
  action


-- |withControlSocket opens a (streaming) unix socket which is bound to
-- a passed path. It promises to remove the binding, and clean up the socket
-- in the event of an exception, or after the action has completed running.
withControlSocket :: String -> (Socket -> IO a) -> IO a
withControlSocket path action = withUnixSocket $ \sock ->
  do
    bracket_ (bind sock $ SockAddrUnix path)
             (removeLink path)
             (action sock)
