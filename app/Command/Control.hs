{-# LANGUAGE OverloadedStrings #-}
module Command.Control
( controller
, actingOn ) where

import Data.ByteString ( ByteString )
import Network.Socket (Socket, listen)

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings, setPort)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import Manager (Environment(..), Command(..), postCommand)

type Controller a = ReaderT Environment IO a

actingOn :: Controller a -> Environment -> IO a
actingOn = runReaderT

controller :: Socket -> Controller ()
controller sock = do
  env <- ask
  liftIO $ do
    listen sock 5
    let settings = setPort 3000 defaultSettings
    runSettingsSocket settings sock (control env)

control :: Environment -> Application
control env _ respond = dispatch `actingOn` env
  where
    dispatch = do
      env <- ask
      liftIO $ do
        postCommand (commandQueue env) (Add undefined undefined)
        respond $ responseLBS status200 [(hContentType, "text/plain")] "Unix socket on vanguard"
