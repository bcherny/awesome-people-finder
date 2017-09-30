{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Github (getRepos) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

import Utils (processResponse)

getContributors :: String -> String -> IO (Response L8.ByteString)
getContributors repoName repoOwner = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest "https://api.github.com/graphql"
  let query = "{\
    \repository(name: \"" ++ repoName ++ "\", owner: \"" ++ repoOwner ++ "\") {\
      \commitComments(first: 100) {\
        \edges {\
          \node {\
            \author {\
              \login\
            \}}}}}}"
  let request = initialRequest {
    method = "POST",
    requestBody = RequestBodyBS $ pack query,
    requestHeaders = [("User-Agent", "Haskell")]
  }
  httpLbs request manager

getRepos :: IO (Maybe Repos)
getRepos = do
  (decodeReposResponse . processResponse) <$> (getRepos' 1)

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

-- TODO: input type should be L8.ByteString (no Maybe)
decodeReposResponse :: Maybe L8.ByteString -> Maybe Repos
decodeReposResponse raw = case raw of
  Just a -> decode a
  Nothing -> Nothing

--------------- types ---------------

data Repos = Repos {
  total_count :: Int,
  incomplete_results :: Bool,
  items :: [Repo]
} deriving (Show, Generic)

instance FromJSON Repos
instance ToJSON Repos

data Repo = Repo {
  name :: String,
  owner :: Owner
} deriving (Show, Generic)

instance FromJSON Repo
instance ToJSON Repo

data Owner = Owner {
  login :: String
} deriving (Show, Generic)

instance FromJSON Owner
instance ToJSON Owner