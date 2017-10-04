{-# LANGUAGE OverloadedStrings #-}

module Utils (logResponse, processResponse) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Debug.Trace (trace)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

logResponse :: Show a => Maybe a -> IO ()
logResponse Nothing = putStrLn "Error"
logResponse (Just a) = putStrLn $ show a

isFailure :: Response L8.ByteString -> Bool
isFailure response
  | (statusCode (responseStatus response)) == 200 = False
  | otherwise = True

processResponse :: Response L8.ByteString -> Maybe L8.ByteString
processResponse response =
  case isFailure response of
    True -> trace ("HTTP error: " ++ show response) Nothing
    False -> Just $ responseBody response