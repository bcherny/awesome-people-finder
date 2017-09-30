{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

import Github (getRepos)
import Utils (logResponse, processResponse)

main :: IO ()
main = do
  response <- getRepos
  logResponse $ processResponse response






