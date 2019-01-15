module Lib where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (($$+-))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Conduit as C
import           Text.XML

someFunc :: IO ()
someFunc = putStrLn "someFunc"


search :: String -> Manager -> ExceptT SomeException IO Document
search query manager = ExceptT $ do
  request <- parseRequest $ "GET https://www.google.com?q=" ++ query
  response <- httpLbs request manager
  let body = responseBody response
  pure $ parseLBS def body

test = do
  manager <- newManager tlsManagerSettings
  runExceptT $ search "hello" manager


x = C.httpLbs
{-
Three APIs are relevant for network request (leaving out wrappers such as
wreq)

1. Network.HTTP.Simple (http-conduit). This is the simplest API. No explicit
managers.

2. Network.HTTP.Conduit (http-conduit). Using conduits. A bit lower level.

3. Network.HTTP.Client (http-client). No conduits. Lowest level.

For parsing HTML documents:

1. xml-conduit
2. html-conduit

-}
