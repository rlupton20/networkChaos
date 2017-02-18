{-# LANGUAGE OverloadedStrings #-}
module Command.Control where
--( controller
--, actingOn ) where

import Data.ByteString ( ByteString )
import Network.Socket (Socket, listen)

import Network.Wai (Application, responseLBS, lazyRequestBody)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings, setPort)
import Network.HTTP.Types (status200, notFound404, notAcceptable406)
import Network.HTTP.Types.Header (hContentType)

import qualified Data.Aeson as A

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Data.Unique (Unique, newUnique, hashUnique)

import Manager (Environment(..), Command(..))

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
    dispatch = liftIO $ do
      raw <- lazyRequestBody request
      js <- maybe (return $ Left 406) action $ (A.decode raw :: Maybe Request)
      case js of
        (Right json) -> respondJSON status200 (A.encode json)
        (Left 404) -> respondJSON notFound404 ""
        (Left 406) -> respondJSON notAcceptable406 ""
        (Left _) -> respondJSON notAcceptable406 ""

    action :: Request -> IO (Either Int Response)
    action cmd = case cmd of
      Develop endpoint -> develop endpoint `actingOn` env
      Connect uid r -> connect uid r `actingOn` env
      New -> new `actingOn` env
      BadRequest -> return $ Left 404

    respondJSON status message =
      respond $ responseLBS status [(hContentType, "application/json")] message


develop :: Connection -> Controller (Either Int Response)
develop connection = do
  env <- ask
  liftIO $ do
    cv <- newCommVar
    let message = Direct connection cv
    message `passTo` (commandQueue env)
    c <- takeCommVar cv
    return . Right $ ConnectingWith c


connect :: Int -> Connection -> Controller (Either Int Response)
connect uid r = do
  env <- ask
  liftIO $ do
    wc <- retrievePending (pending env) uid
    case wc of
      Just (PC pc) -> do
        putCommVar pc r
        return . Right $ OK
      Nothing -> return . Left $ 404


new :: Controller (Either Int Response)
new = do
  cq <- asks commandQueue
  liftIO $ do
    uid <- fmap hashUnique newUnique
    cv <- newCommVar
    let message = Create uid cv
    message `passTo` cq
    c <- takeCommVar cv
    return . Right $ ListeningOn uid c
