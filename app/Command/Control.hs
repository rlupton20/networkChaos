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
        js <- maybe (return $ Left 406) action $ (A.decode raw :: Maybe Request)
        case js of
          (Right json) -> respondJSON status200 (A.encode json)
          (Left 404) -> respondJSON status404 ""

    action :: Request -> IO (Either Int Response)
    action cmd = case cmd of
      Develop endpoint -> develop endpoint `actingOn` env
      Connect uid r -> connect uid r `actingOn` env
      New -> new `actingOn` env
      BadRequest -> return $ Left 404

    respondJSON status message =
      respond $ responseLBS status [(hContentType, "application/json")] message


develop :: Connection -> Controller (Either Int Response)
develop endpoint = do
  env <- ask
  liftIO $ withProtectedBoundUDPSocket $ \sock -> do
    vip <- getAddr (routingTable env)
    Just (gip, p) <- describeSocket sock
    let c = Connection vip gip (fromIntegral p)
        message = Add (virtualip endpoint) (ip endpoint, fromIntegral $ port endpoint) sock
    postCommand (commandQueue env) message
    return . Right $ ListeningOn 0 c

connect :: Int -> Connection -> Controller (Either Int Response)
connect uid r = do
  env <- ask
  liftIO $ do
    lu <- retrievePending (pending env) uid
    case lu of
      (Just (PC l s)) -> do
        let message = Add (virtualip r) (ip r, fromIntegral $ port r) s
        postCommand (commandQueue env) message
        return . Right $ OK
      Nothing -> return $ Left 404

new :: Controller (Either Int Response)
new = do
  env <- ask
  liftIO $ withProtectedBoundUDPSocket $ \sock -> do
    vip <- getAddr (routingTable env)
    Just (gip, p) <- describeSocket sock
    let c = Connection vip gip (fromIntegral p)
    uid <- hashUnique <$> newUnique
    addPending (pending env) uid $ PC c sock
    return . Right $ ListeningOn uid c


