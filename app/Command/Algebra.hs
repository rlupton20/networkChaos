{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Command.Algebra where

import Network.Socket (Socket, PortNumber)
import Control.Monad.Free.Church (MonadFree, liftF)
import Types (Addr)

data User = User

-- |AConnection is the operational algebra for establishing new connections
data AConnection a = NewConnection User (Socket -> a)
                   | STUN Socket ((Addr, PortNumber) -> a)
                   | Connect User (Addr, PortNumber) (Either String () -> a)
                   | Remove User (() -> a)

instance Functor AConnection where
  fmap f (NewConnection u g) = NewConnection u (f . g)
  fmap f (STUN s g) = STUN s (f . g)
  fmap f (Connect u e g) = Connect u e (f . g)
  fmap f (Remove u g) = Remove u (f . g)

-- |IConnection is typeclass representing the interface provided by
-- something (a monad) which implements operations for the operational
-- algebra AConnection
class IConnection f where
  newConnection :: User -> f Socket
  stun :: Socket -> f (Addr, PortNumber)
  connect :: User -> (Addr, PortNumber) -> f (Either String ())
  remove :: User -> f ()

-- |AConnection is designed to implement IConnection
instance IConnection AConnection where
  newConnection u = NewConnection u id
  stun s = STUN s id
  connect u e = Connect u e id
  remove u = Remove u id

-- |Any monad which is free over a functor implementing IConnection
-- is automatically an instance of IConnection
instance (IConnection f, Functor f, MonadFree f m) => IConnection m where
  newConnection u = liftF $ newConnection u
  stun s = liftF $ stun s
  connect u e = liftF $ connect u e
  remove u = liftF $ remove u


newSTUNSocket :: (Monad m, IConnection m) => User -> m (Addr, PortNumber)
newSTUNSocket = undefined
