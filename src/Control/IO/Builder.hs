{-# LANGUAGE RecordWildCards #-}
module Control.IO.Builder
( Buildable
, build 
, WorkQueue
, Worker
, newWorker
, passWork
, WorkSource
, makeWorkSourceOf ) where

import Control.IO.Builder.Worker
import Control.IO.Builder.WorkSource

import Control.Monad

class Buildable a where
  build :: a -> IO ()

instance Buildable (IO a) where
  build io = io >> return ()

instance Buildable (Worker a) where
  build Worker{..} = forever $ do
    job <- readWorkQueue wq
    action job

instance Buildable (WorkSource a) where
  build WorkSource{..} = forever $ do
    job <- source
    job `passWork` worker
