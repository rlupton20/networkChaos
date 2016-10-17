{-# LANGUAGE OverloadedStrings #-}
module Command.Control
( controller ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (bracket, bracket_)
import Network.Socket ( Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix)
                      , Socket, defaultProtocol, socket, bind, close
                      , listen, accept )
import Network.Socket.ByteString ( send, recv )
import System.Posix.Files ( removeLink )

import Data.ByteString ( ByteString )
import Network.TCP ( socketConnection )
import Network.HTTP ( HandleStream, receiveHTTP, Request(rqBody) )

import Command.ControlTypes (Control, ControlEnvironment)
  

controller :: String -> ControlEnvironment -> IO ()
controller socketPath _ = do
  liftIO $ withControlSocket socketPath $ \sock ->
    do 
        listen sock 5
        serverLoop sock
        

serverLoop :: Socket -> IO ()
serverLoop sock = do
  (conn, _) <- accept sock

  hs <- (socketConnection "Client" 0 conn :: IO (HandleStream ByteString))
  req <- receiveHTTP hs
  case req of
    Right request -> if (rqBody request) == "quit" then return () else serverLoop sock
    Left _ -> putStrLn "Error"


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
