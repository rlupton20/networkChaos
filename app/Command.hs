module Command
( routeMaster ) where

import Control.Monad.IO.Class (liftIO)
import Network.Socket ( Socket, PortNumber, SockAddr
                      , AddrInfo(..), getAddrInfo, close)
import Network.Socket.ByteString (sendTo, recv)
import Control.Monad (forever)
import Control.Concurrent.Async (race_)

import Routing.RoutingTable (withRoute, (#->), getInjector, getAddr)

import Command.Types (errorBracketedPending)
import Command.STUN

import Manager
import Core

-- |commander is a Manager process which takes commands and
-- spawns submanagers to execute those commands.
routeMaster :: Manager ()
routeMaster = do
  cq <- withEnvironment commandQueue
  next <- liftIO $ readQueue cq
  case next of
    Quit -> return ()
    (Create uid cv) -> spawn (new uid cv) >> routeMaster
    (Direct c cv) -> spawn (direct c cv) >> routeMaster
    (Remove a) -> spawn (remove a) >> routeMaster


direct :: Connection -> CommVar Connection -> Manager ()
direct (Connection l r p) cv = do
  env <- environment
  liftIO $ withProtectedBoundUDPSocket $ \sock -> do
    q <- newQueue
    c <- describeConnection env sock
    withRoute (routingTable env) (l #-> (r,q)) $ do
      putCommVar cv c
      makeRelay sock (getInjector . routingTable $ env) q (r,p)


makeRelay :: Socket -> Injector -> PacketQueue -> (Addr, PortNumber) -> IO ()
makeRelay s inj q (a,p) = do
  let dest = buildAddress a p
  race_ (dispatch q dest) (inject s)
  where
    dispatch q dest = forever $ do
      bs <- readQueue q
      sendTo s bs dest

    inject s = forever $ do
      bs <- recv s 4096
      bs `passTo` inj


new :: Int -> CommVar Connection -> Manager ()
new uid cv = do
  env <- environment
  let p = pending env
  liftIO $ withProtectedBoundUDPSocket $ \sock -> do
    (Connection v a p) <- errorBracketedPending p uid $ \pc -> do
      c <- describeConnection env sock
      putCommVar cv c
      takeCommVar pc
    q <- newQueue
    withRoute (routingTable env) (v #-> (a,q)) $
      makeRelay sock (getInjector . routingTable $ env) q (a,p)


remove :: Addr -> Manager ()
remove _ = liftIO $ putStrLn "remove"


describeConnection :: Environment -> Socket -> IO Connection
describeConnection env s  = do
  l <- getAddr (routingTable env)
  (a, p) <- describeSocket s
  return $ Connection l a p
