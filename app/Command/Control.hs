{-# LANGUAGE OverloadedStrings #-}
module Command.Control where
--( controller
--, actingOn ) where

import Data.ByteString ( ByteString )
import Network.Socket (Socket, listen)

import Network.Wai (Application, responseLBS, lazyRequestBody)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings, setPort)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)

import qualified Data.Aeson as A
import Control.Applicative ((<$>))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Unique (Unique, newUnique, hashUnique)

import Manager (Environment(..), Command(..), postCommand)
import Routing.RoutingTable (getAddr)
import Core

import Command.Types

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
                New endpoint -> new endpoint `actingOn` env
                Connect uid -> connect uid `actingOn` env
                BadRequest -> return $ Left 404
        case js of
          (Right json) -> respondJSON status200 (A.encode json)
          (Left 404) -> respondJSON status404 ""

    respondJSON status message =
      respond $ responseLBS status [(hContentType, "application/json")] message

new :: Connection -> Controller (Either Int Response)
new endpoint = do
  env <- ask
  liftIO $ withProtectedBoundUDPSocket $ \sock -> do
    ip <- getAddr (routingTable env)
    Just (vip, p) <- describeSocket sock
    uid <- hashUnique <$> newUnique
    let c = Connection ip vip (fromIntegral p)
    addPending (pending env) uid $ PC endpoint c sock
    return . Right $ ListeningOn uid c

connect :: Int -> Controller (Either Int Response)
connect uid = do
  env <- ask
  liftIO $ do
    lu <- retrievePending (pending env) uid
    case lu of
      (Just (PC r l s)) -> do
        let message = Add (virtualip r) (ip r, fromIntegral $ port r) s
        postCommand (commandQueue env) message
        return . Right $ OK
      Nothing -> return $ Left 404



