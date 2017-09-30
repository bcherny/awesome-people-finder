{-# LANGUAGE OverloadedStrings #-}

module Github (getRepos) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

import Utils (processResponse)

getRepos :: IO (Response L8.ByteString)
getRepos = do
  getRepos' 1

  where
    getRepos' :: Int -> IO (Response L8.ByteString)
    getRepos' page = do
      manager <- newManager tlsManagerSettings
      initialRequest <- parseRequest "https://api.github.com/search/repositories"
      let request = initialRequest {
        requestHeaders = [("User-Agent", "Haskell")],
        queryString = "q=language:typescript&sort=stars"
      }
      httpLbs request manager
