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

import Command.ControlTypes ( Control, ControlEnvironment(..)
                            , forkWith, environment, withEnvironment, local )

import Data.String ( IsString ) -- Will be removed when API is properly written


controller :: Control ()
controller = do
    sock <- withEnvironment controlSocket
    liftIO $ listen sock 5
    server
    

server :: Control ()
server = do
  sock <- withEnvironment controlSocket
  loop sock
  where
    loop :: Socket -> Control ()
    loop sock = do
      (conn, _) <- liftIO $ accept sock
      workFromSocket conn $ forkWith process
      loop sock

    workFromSocket :: Socket -> Control a -> Control a
    workFromSocket sock ctl = do
      env <- environment
      local (const $ env { controlSocket = sock }) ctl
  

process :: Control ()
process = do
  sock <- withEnvironment controlSocket
  liftIO $ do
    hs <- socketConnection "Client" 0 sock :: IO (HandleStream ByteString)
    req <- receiveHTTP hs
    case req of
      Right request -> putStrLn $ "Got request " ++ show request
      Left _ -> putStrLn "Error"
