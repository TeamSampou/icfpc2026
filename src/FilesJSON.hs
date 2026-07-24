{-# OPTIONS -Wno-missing-export-lists #-}

module FilesJSON where

import qualified Data.ByteString.Lazy.Char8 as L8
import System.FilePath ((</>), (<.>))

import Data.Aeson

import TypesJSON

loadProbStatList :: IO [ProbStat]
loadProbStatList = do
  bs <- L8.readFile "list-problem.json"
  either fail pure $ eitherDecode bs

getDownloadCmdLines :: IO [String]
getDownloadCmdLines = do
  pss <- loadProbStatList
  pure [cmd $ pstatSlug ps | ps <- pss]
  where
    cmd pslug = unwords ["./api/fetch-prob.sh", pslug, ">", "problems" </> pslug <.> "json"]
