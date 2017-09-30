{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest "https://api.github.com/search/repositories"
  let request = setQueryString [("q", Just "language:typescript")] initialRequest {
    requestHeaders = [("User-Agent", "Haskell")]
  }
  response <- httpLbs request manager
  L8.putStrLn $ responseBody response
