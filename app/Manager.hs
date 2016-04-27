module Manager
( Environment(..)
, makeManaged
, Manager
, manage
, spawn
, environment
, fromEnvironment
{-, tryManager
, maskManager-} ) where

import Manager.Manager
import Manager.Manage
import Manager.Types
