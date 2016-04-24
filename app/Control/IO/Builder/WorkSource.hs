{-# LANGUAGE RecordWildCards #-}
module Control.IO.Builder.WorkSource where

import Control.IO.Builder.Worker

data WorkSource a = WorkSource { source :: IO a
                               , worker :: Worker a}

makeWorkSourceOf :: IO a -> Worker a -> WorkSource a
makeWorkSourceOf s w = WorkSource s w
