{-# OPTIONS -Wno-missing-export-lists #-}

module FilesJSON where

import qualified Data.ByteString.Lazy.Char8 as L8
import System.FilePath ((</>), (<.>))
import System.IO.Error

import Data.Aeson

import TypesJSON

loadProbStatList :: IO [ProbStat]
loadProbStatList = do
  bs <- L8.readFile "list-problem.json"
  either fail pure $ eitherDecode bs

type PSlug = String

problemPath :: PSlug -> FilePath
problemPath pslug = "problems" </> pslug <.> "json"

getDownloadCmdLines :: IO [String]
getDownloadCmdLines = do
  pss <- loadProbStatList
  pure [cmd $ pstatSlug ps | ps <- pss]
  where
    cmd pslug = unwords ["./api/fetch-prob.sh", pslug, ">", problemPath pslug]

loadProblem :: PSlug -> IO Problem
loadProblem pslug = do
  bs <- L8.readFile $ problemPath pslug
  either (\e -> fail (pslug ++ ":" ++ e)) pure $ eitherDecode bs

loadProblemList' :: IO [Either IOError Problem]
loadProblemList' = do
  pss <- loadProbStatList
  sequence [tryIOError $ loadProblem $ pstatSlug ps | ps <- pss]

loadProblemList :: IO [Problem]
loadProblemList = do
  pss <- loadProbStatList
  sequence [loadProblem $ pstatSlug ps | ps <- pss]
