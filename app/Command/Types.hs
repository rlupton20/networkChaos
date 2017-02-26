{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Command.Types where

import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Aeson ((.:), (.=))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import GHC.Generics (Generic)
import Network.Socket (Socket, PortNumber)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar ( TVar, newTVarIO
                                   , modifyTVar', readTVar, writeTVar)
import Control.Exception (bracketOnError)

import Network.Vanguard.Core

-- |A partial connection represents a conncection which has been
-- started, but for which the details have yet to be finalized.
data PartialConnection = PC { connection :: CommVar Connection }
                            
-- |PartialConnections will be stored in a Map with a unique
-- identifier.
type PendingM = HM.HashMap Int PartialConnection

-- |We wrap the PendingM and give it a basic API.
newtype Pending = Pending (TVar PendingM)

-- | Create a new Pending value.
newPending :: IO Pending
newPending = fmap Pending . newTVarIO $ HM.empty

-- | Add an entry to a collection of Pendings.
addBlankPending :: Pending -> Int -> IO (CommVar Connection)
addBlankPending (Pending p) uid = do
  c <- newCommVar
  atomically $ modifyTVar' p (HM.insert uid (PC c))
  return c

errorBracketedPending :: Pending -> Int -> (CommVar Connection -> IO a) -> IO a
errorBracketedPending pen@(Pending p) uid action =
  bracketOnError (addBlankPending pen uid)
                 (\_ -> atomically $ HM.delete uid <$> readTVar p)
                 action

-- | Retrieve an entry from a Pending. This will delete it from the record.
retrievePending :: Pending -> Int -> IO (Maybe PartialConnection)
retrievePending (Pending p) uid  = atomically $ do
  map <- readTVar p
  let (map', pc) = takeOut map uid
  case pc of
    (Just _) -> writeTVar p map' >> return pc
    Nothing -> return Nothing

-- |Utility function to look up a value, and if it exists, remove it
-- and return it.
takeOut :: HM.HashMap Int a -> Int -> (HM.HashMap Int a, Maybe a)
takeOut m k = case (HM.lookup k m) of
  Just x -> let m' = HM.delete k m in
    (m', Just x)
  Nothing -> (m, Nothing)
