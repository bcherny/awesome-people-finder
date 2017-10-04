{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Github (getContributors, getRepos, getUserData) where

import GHC.Generics
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode, object, Value(Object), (.=), (.:), withObject)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (nub)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Control.Monad (mzero)

import Utils (processResponse)

--------------- getUserData ---------------

getUserData :: String -> String -> IO (Maybe (String, String, String))
getUserData token username = do
  -- print a
  flattenUserDataResponse . decodeUserDataResponse . processResponse <$> getUserData'

  where
    getUserData' :: IO (Response L8.ByteString)
    getUserData' = do
      manager <- newManager tlsManagerSettings
      initialRequest <- parseRequest "https://api.github.com/graphql"

      -- TODO: generate GraphQL from DSL
      let query = "{\"query\":\"query {user(login: \\\"" ++ username ++ "\\\") {email, location, login }}\",\"variables\":{}}"

      let request = initialRequest {
        method = "POST",
        requestBody = RequestBodyLBS $ L8.pack query,
        requestHeaders = [
          ("Authorization", pack $ "Bearer " ++ token),
          ("User-Agent", "Haskell")
        ]
      }
      httpLbs request manager

-- TODO: input type should be Contributors (no Maybe)
flattenUserDataResponse :: Maybe Location -> Maybe (String, String, String)
flattenUserDataResponse Nothing = Nothing
flattenUserDataResponse (Just cs) = Just (login' u, email u, location u)
  where u = user $ locationData' cs

-- TODO: input type should be L8.ByteString (no Maybe)
decodeUserDataResponse :: Maybe L8.ByteString -> Maybe Location
decodeUserDataResponse Nothing = Nothing
decodeUserDataResponse (Just raw) = decode raw

--------------- getContributors ---------------

getContributors :: String -> String -> String -> IO [String]
getContributors token repoName repoOwner = do
  -- print a
  nub . flattenContributorsResponse . decodeContributorsResponse . processResponse <$> getContributors'

  where
    getContributors' :: IO (Response L8.ByteString)
    getContributors' = do
      manager <- newManager tlsManagerSettings
      initialRequest <- parseRequest "https://api.github.com/graphql"

      -- TODO: generate GraphQL from DSL
      let query = "{\"query\":\"query {repository(name: \\\"" ++ repoName ++ "\\\", owner: \\\"" ++ repoOwner ++ "\\\") { commitComments(first: 100) { edges { node { author { login }}}}}}\",\"variables\":{}}"

      let request = initialRequest {
        method = "POST",
        requestBody = RequestBodyLBS $ L8.pack query,
        requestHeaders = [
          ("Authorization", pack $ "Bearer " ++ token),
          ("User-Agent", "Haskell")
        ]
      }
      -- print query
      -- print request
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

getRepos :: String -> IO (Maybe [(String, String)])
getRepos token = do
  (flattenReposResponse . decodeReposResponse . processResponse) <$> (getRepos' 1)

  where
    getRepos' :: Int -> IO (Response L8.ByteString)
    getRepos' page = do
      manager <- newManager tlsManagerSettings
      initialRequest <- parseRequest "https://api.github.com/search/repositories"
      let request = initialRequest {
        requestHeaders = [
          ("Authorization", pack $ "Bearer " ++ token),
          ("User-Agent", "Haskell")
        ],
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
  owner :: Actor
} deriving (Show, Generic)

instance FromJSON Repo
instance ToJSON Repo

data Actor = Actor {
  login :: String
} deriving (Show, Generic)

instance FromJSON Actor
instance ToJSON Actor

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
  author :: Actor
} deriving (Show, Generic)

instance FromJSON CommentNode
instance ToJSON CommentNode

------

-- UGLY! See https://ghc.haskell.org/trac/ghc/wiki/Records
data Location = Location {
  locationData' :: LocationData
} deriving (Show)

instance FromJSON Location where
  parseJSON (Object x) = Location <$> x .: "data"
  parseJSON _ = mzero

instance ToJSON Location where
  toJSON (Location locationData') = object
    [ "data" .= locationData']

data LocationData = LocationData {
  user :: User
} deriving (Generic, Show)
instance FromJSON LocationData
instance ToJSON LocationData

data User = User {
  email :: String,
  location :: String,
  login' :: String
} deriving (Show)

instance FromJSON User where
  parseJSON (Object x) = User <$> x .: "email"
                              <*> x .: "location"
                              <*> x .: "login"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON (User email location login') = object
    [ "email" .= email, "location" .= location, "login" .= login']