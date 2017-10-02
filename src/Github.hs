{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Github (getContributors, getRepos) where

import GHC.Generics
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode, object, Value(Object), (.=), (.:), withObject)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Control.Monad (mzero)

import Utils (processResponse)

--------------- getContributors ---------------

getContributors :: String -> String -> IO [String]
getContributors repoName repoOwner = do

  (flattenContributorsResponse . decodeContributorsResponse . processResponse) <$> getContributors'

  where
    getContributors' :: IO (Response L8.ByteString)
    getContributors' = do
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

-- TODO: input type should be Contributors (no Maybe)
flattenContributorsResponse :: Maybe Contributors -> [String]
flattenContributorsResponse Nothing = []
flattenContributorsResponse (Just cs) = (map (\e -> login $ author $ node e) (edges $ commitComments $ repository $ data' cs))

-- TODO: input type should be L8.ByteString (no Maybe)
decodeContributorsResponse :: Maybe L8.ByteString -> Maybe Contributors
decodeContributorsResponse Nothing = Nothing
decodeContributorsResponse (Just raw) = decode raw

--------------- getRepos ---------------

getRepos :: IO (Maybe [(String, String)])
getRepos = do
  (flattenReposResponse . decodeReposResponse . processResponse) <$> (getRepos' 1)

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

-- TODO: input type should be Repos (no Maybe)
flattenReposResponse :: Maybe Repos -> Maybe [(String, String)]
flattenReposResponse Nothing = Nothing
flattenReposResponse (Just repos) = Just (map (\r -> (name r, login $ owner r)) (items repos))

-- TODO: input type should be L8.ByteString (no Maybe)
decodeReposResponse :: Maybe L8.ByteString -> Maybe Repos
decodeReposResponse Nothing = Nothing
decodeReposResponse (Just raw) = decode raw

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
  owner :: User
} deriving (Show, Generic)

instance FromJSON Repo
instance ToJSON Repo

data User = User {
  login :: String
} deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data Contributors = Contributors {
  data' :: ContributorsData
} deriving (Show)

instance FromJSON Contributors where
  parseJSON (Object x) = Contributors <$> x .: "data"
  parseJSON _ = mzero

instance ToJSON Contributors where
  toJSON (Contributors data') = object
    [ "data" .= data']

data ContributorsData = ContributorsData {
  repository :: Repository
} deriving (Show, Generic)

instance FromJSON ContributorsData
instance ToJSON ContributorsData

data Repository = Repository {
  commitComments :: CommitComments
} deriving (Show, Generic)

instance FromJSON Repository
instance ToJSON Repository

data CommitComments = CommitComments {
  edges :: [CommentEdge]
} deriving (Show, Generic)

instance FromJSON CommitComments
instance ToJSON CommitComments

data CommentEdge = CommentEdge {
  node :: CommentNode
} deriving (Show, Generic)

instance FromJSON CommentEdge
instance ToJSON CommentEdge

data CommentNode = CommentNode {
  author :: User
} deriving (Show, Generic)

instance FromJSON CommentNode
instance ToJSON CommentNode