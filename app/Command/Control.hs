{-# LANGUAGE OverloadedStrings #-}
module Command.Control
( controller ) where

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Network.Socket ( Socket, listen, accept )
import Network.Socket.ByteString ( send, recv )

import Data.ByteString ( ByteString )
import Network.TCP ( socketConnection )
import Network.HTTP ( HandleStream, receiveHTTP, Request(rqBody) )

import Command.ControlTypes ( Control, ControlEnvironment(..), withEnvironment )

import Data.String ( IsString ) -- Will be removed when API is properly written


controller :: Control ()
controller = do
    sock <- withEnvironment controlSocket
    liftIO $ do
        listen sock 5
        serverLoop sock
        

serverLoop :: Socket -> IO ()
serverLoop sock = do
  (conn, _) <- accept sock

  hs <- socketConnection "Client" 0 conn :: IO (HandleStream ByteString)
  req <- receiveHTTP hs
  case req of
    Right request -> unless (isQuit request) $ serverLoop sock
    Left _ -> putStrLn "Error"


isQuit :: (Eq a, IsString a) => Request a -> Bool
isQuit request = rqBody request == "quit"
