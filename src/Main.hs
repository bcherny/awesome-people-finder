module Main where

import Data.List (nub)
import System.Environment (lookupEnv)

import Github (getContributors, getRepos, getUserData)
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
          print $ take 1 r
          contribs <- get t $ take 1 r    -- [String]
          print contribs

          us <- mapM (getUserData t) (take 5 contribs)

          print us

          -- case u of
          --   Just t -> print t
          --   Nothing -> return ()
        Nothing -> return ()

get :: String -> [(String, String)] -> IO [String]
get token repos = fmap (nub . concat) $ mapM (uncurry $ getContributors token) repos
