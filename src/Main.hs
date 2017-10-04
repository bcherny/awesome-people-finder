module Main where

import Data.List (nub)
import System.Environment (lookupEnv)

import Github (getContributors, getRepos)
import Utils (logResponse)

main :: IO ()
main = do

  token <- lookupEnv "GITHUB_TOKEN"

  case token of
    Nothing -> error "Please define env var GITHUB_TOKEN"
    Just t -> do

      repos <- getRepos t           -- Maybe [(String, String)]

      case repos of
        Just r -> do
          print $ take 3 r
          cs <- get t $ take 3 r    -- [String]
          print cs
        Nothing -> return ()

get :: String -> [(String, String)] -> IO [String]
get token repos = fmap (nub . concat) $ mapM (uncurry $ getContributors token) repos
