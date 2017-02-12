{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Command.Types where

import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar ( TVar, newTVarIO
                                   , modifyTVar', readTVar, writeTVar)

import Core

data Connection = Connection { virtualip :: Addr
                             , ip :: Addr
                             , port :: Int } deriving (Generic, Eq, Show)

instance A.FromJSON Connection
instance A.ToJSON Connection

data Request = New Connection | Connect Int | BadRequest deriving (Eq, Show)
data Response = ListeningOn Int Connection | OK deriving (Show)

instance A.FromJSON Request where
  parseJSON (A.Object o) = case HM.lookup "request" o of
    Just (A.String "new") -> case HM.lookup "endpoint" o of
      Just (A.Object _) -> New <$> o .: "endpoint"
      _ -> pure BadRequest
    Just (A.String "connect") -> case HM.lookup "uid" o of
      Just (A.Number _) -> Connect <$> o .: "uid"
      _ -> pure BadRequest
    _ -> pure BadRequest

instance A.ToJSON Response where
  toJSON (ListeningOn uid connection) = A.object $
    [ "listening" .= A.Number (fromIntegral uid)
    , "endpoint" .= A.toJSON connection ]
  toJSON OK = A.String "OK"




data PartialConnection = PC { remote :: Connection
                            , local :: Connection
                            , sock :: Socket }

type PendingM = HM.HashMap Int PartialConnection

newtype Pending = Pending (TVar PendingM)

newPending :: IO Pending
newPending = fmap Pending . newTVarIO $ HM.empty

addPending :: Pending -> Int -> PartialConnection -> IO ()
addPending (Pending p) uid pc  = atomically $ modifyTVar' p (HM.insert uid pc)

retrievePending :: Pending -> Int -> IO (Maybe PartialConnection)
retrievePending (Pending p) uid  = atomically $ do
  m <- readTVar p
  let (m', pcm) = swapOut m uid
  case pcm of
    (Just pc) -> writeTVar p m' >> return pcm
    Nothing -> return Nothing

swapOut :: HM.HashMap Int a -> Int -> (HM.HashMap Int a, Maybe a)
swapOut m k = case (HM.lookup k m) of
  (Just x) -> let m' = HM.delete k m in
    (m', Just x)
  Nothing -> (m,Nothing)
