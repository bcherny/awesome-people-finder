{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)

import Github (getContributors, getRepos)
import Utils (logResponse)

main :: IO ()
main = do

  repos <- getRepos           -- Maybe [(String, String)]

  case repos of
    Just r -> do
      print $ take 1 r
      cs <- get (take 1 r)    -- [String]
      print cs
    Nothing -> return ()

get :: [(String, String)] -> IO [String]
get repos = fmap concat $ mapM (uncurry getContributors) repos
