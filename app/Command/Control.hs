{-# LANGUAGE OverloadedStrings #-}
module Command.Control
( controller ) where

import Data.ByteString ( ByteString )
import Network.Socket (Socket, listen)

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings, setPort)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Control.Monad.IO.Class (liftIO)

import Manager

controller :: Socket ->  Manager ()
controller sock = liftIO $ do
    listen sock 5
    let settings = setPort 3000 defaultSettings
    runSettingsSocket settings sock control

control :: Application
control _ respond = respond $
  responseLBS status200 [(hContentType, "text/plain")] "Unix socket on vanguard"
