{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Command.Control where
--( controller
--, actingOn ) where

import Data.ByteString ( ByteString )
import Network.Socket (Socket, listen)

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings, setPort)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import Control.Applicative ((<$>))
import GHC.Generics (Generic)
-- import Data.Text (Text)
import qualified Data.HashMap.Lazy as HM

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import Manager (Environment(..), Command(..), postCommand)
import Core

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
        respond $ responseLBS status200 [(hContentType, "text/plain")] js

    (Just a) = addr "0.0.0.0"
    (Just b) = addr "1.1.1.1"

    js = A.encode $ Connection a b 10

data Connection = Connection { virtualip :: Addr
                             , ip :: Addr
                             , port :: Int } deriving (Generic, Eq, Show)

instance A.FromJSON Connection
instance A.ToJSON Connection

data Request = New Connection | BadRequest deriving (Eq, Show)
data Response = ListeningOn Connection deriving (Show)

instance A.FromJSON Request where
  parseJSON (A.Object o) = case HM.lookup "request" o of
    Just (A.String "new") -> case HM.lookup "endpoint" o of
      Just (A.Object _) -> New <$> o .: "endpoint"
      _ -> pure BadRequest
    _ -> pure BadRequest
