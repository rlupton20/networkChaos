{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Command.Control where
--( controller
--, actingOn ) where

import Data.ByteString ( ByteString )
import Network.Socket (Socket, listen)

import Network.Wai (Application, responseLBS, lazyRequestBody)
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
import Routing.RoutingTable (getAddr)
import Core

import Network.Socket (close) --temp

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
control env request respond = dispatch `actingOn` env
  where
    dispatch = do
      env <- ask
      liftIO $ do
        raw <- lazyRequestBody request
        let (Just cmd) = A.decode raw :: Maybe Request
        js <- case cmd of
                New endpoint -> withProtectedBoundUDPSocket $ \sock -> do
                  ip <- getAddr (routingTable env)
                  Just (vip, p) <- describeSocket sock
                  let json = A.encode $ Connection ip vip (fromIntegral p)
                  close sock
                  return json 
        postCommand (commandQueue env) (Add undefined undefined)
        respond $ responseLBS status200 [(hContentType, "text/plain")] js

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
