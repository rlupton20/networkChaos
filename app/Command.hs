module Command
( routeMaster ) where

import Control.Monad.IO.Class (liftIO)
import Network.Socket ( Socket, PortNumber, SockAddr
                      , AddrInfo(..), getAddrInfo, close)
import Network.Socket.ByteString (sendTo, recv)
import Control.Exception (bracket_)
import Control.Monad (forever)
import Control.Concurrent.Async (race_)

import Routing.RoutingTable (withRoute, (#->), getInjector)

import Manager
import Core

-- |commander is a Manager process which takes commands and
-- spawns submanagers to execute those commands.
routeMaster :: Manager ()
routeMaster = do
  cq <- withEnvironment commandQueue
  next <- liftIO $ getCommand cq
  case next of
    Quit -> return ()
    (Add l r s) -> spawn (add l r s) >> routeMaster
    (Remove a) -> spawn (remove a) >> routeMaster

add :: Addr -> (Addr, PortNumber) -> Socket -> Manager ()
add l e@(r,p) s = do
  env <- environment
  liftIO $ covering s $ do
    q <- newQueue
    withRoute (routingTable env) (l #-> (r,q)) $
      makeRelay s (getInjector . routingTable $ env) q e
  where
    covering s action = bracket_ (return ())
      (close s)
      action

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
      

remove :: Addr -> Manager ()
remove _ = liftIO $ putStrLn "remove"
