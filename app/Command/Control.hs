{-# LANGUAGE OverloadedStrings #-}
module Command.Control
( controller ) where

import Data.ByteString ( ByteString )
import Network.Socket (Socket, listen)

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings, setPort)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Manager (Command)

controller :: Socket -> (Command -> IO ()) ->  IO ()
controller sock post = do
    listen sock 5
    let settings = setPort 3000 defaultSettings
    runSettingsSocket settings sock (control post)

control :: (Command -> IO ()) -> Application
control post _ respond = respond $
  responseLBS status200 [(hContentType, "text/plain")] "Unix socket on vanguard"


