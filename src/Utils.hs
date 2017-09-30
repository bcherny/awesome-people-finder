{-# LANGUAGE OverloadedStrings #-}

module Utils (logResponse, processResponse) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

logResponse :: Maybe L8.ByteString -> IO ()
logResponse Nothing = L8.putStrLn "Error"
logResponse (Just a) = L8.putStrLn a

isFailure :: Response L8.ByteString -> Bool
isFailure response
  | (statusCode (responseStatus response)) == 200 = False
  | otherwise = True

processResponse :: Response L8.ByteString -> Maybe L8.ByteString
processResponse response = case isFailure response of
  True -> Nothing
  False ->
    Just $ responseBody response