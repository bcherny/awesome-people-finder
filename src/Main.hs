{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
  response <- getRepos
  logResponse (processResponse response)

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

processResponse :: Response L8.ByteString -> Maybe L8.ByteString
processResponse response = case isFailure response of
  True -> Nothing
  False ->
    Just $ responseBody response

logResponse :: Maybe L8.ByteString -> IO ()
logResponse Nothing = L8.putStrLn "Error"
logResponse (Just a) = L8.putStrLn a

isFailure :: Response L8.ByteString -> Bool
isFailure response
  | (statusCode (responseStatus response)) == 200 = False
  | otherwise = True
