module Main where

import Data.List (nub)

import Github (getContributors, getRepos)
import Utils (logResponse)

main :: IO ()
main = do

  -- a <- get [("vscode","Microsoft")]
  -- print a

  repos <- getRepos           -- Maybe [(String, String)]

  case repos of
    Just r -> do
      print $ take 3 r
      cs <- get $ take 3 r    -- [String]
      print cs
    Nothing -> return ()

get :: [(String, String)] -> IO [String]
get repos = fmap (nub . concat) $ mapM (uncurry getContributors) repos
