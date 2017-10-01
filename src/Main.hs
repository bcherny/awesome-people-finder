{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)

import Github (getContributors, getRepos)
import Utils (logResponse)

main :: IO ()
main = do
  repos <- getRepos

  case repos of
    Just rs -> do
      cs <- map (\(a, b) -> getContributors a b) repos
      logResponse cs
    Nothing -> Nothing

  -- repos <- getRepos
  -- fmap (\r -> map (\(a, b) -> getContributors a b)) repos


-- get :: (Maybe [(String, String)]) -> Maybe [String]
-- get m = m >>= (\as -> map (\(a, b) -> getContributors a b)) as




