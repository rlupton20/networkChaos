{-# LANGUAGE RecordWildCards #-}
module Data.DHT.Etcd where

import Network.HTTP
import Network.URI (parseURI)
import Data.Aeson

import Control.Applicative

data Etcd = Etcd { restAddress :: String }
type Key = String

get :: Etcd -> Key -> IO String
get Etcd{..} key = do
  simpleHTTP (getRequest $ "http://" ++ restAddress ++ "/v2/keys" ++ key) >>= getResponseBody

putRequest :: String -> String -> Maybe (Request String)
putRequest place what = let uri = parseURI place
                            empty = fmap (mkRequest PUT) uri in
  fmap (\c -> setRequestBody c ("application/x-www-form-urlencoded", what)) empty
  
put :: Etcd -> Key -> String -> IO String
put Etcd{..} key value = do
  let url = "http://" ++ restAddress ++ "/v2/keys" ++ key
      mreq = putRequest url $ "value=" ++ value
  case mreq of
    Just req -> simpleHTTP req >>= getResponseBody
    Nothing -> return "Not a valid URI for PUT command."
