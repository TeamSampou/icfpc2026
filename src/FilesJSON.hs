{-# OPTIONS -Wno-missing-export-lists #-}

module FilesJSON where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import Data.Time
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

import Data.Aeson

import TypesJSON

loadProbStatList :: IO [ProbStat]
loadProbStatList = do
  bs <- L8.readFile "list-problem.json"
  either fail pure $ eitherDecode bs

type PSlug = String

problemPath :: PSlug -> FilePath
problemPath pslug = "problems" </> pslug <.> "json"

solutionDir :: FilePath
solutionDir = "solutions/man"

transactionDir :: FilePath
transactionDir = "transactions"

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

lookupProbStat :: Eq a => (ProbStat -> a) -> a -> [ProbStat] -> Maybe ProbStat
lookupProbStat f x = find (\p -> f p == x)

submit' :: ProbStat -> IO ()
submit' ps = do
      hPutStrLn stderr $ "** generating submission request for '" ++ pslug ++ "'"
      json <- getSubmit
      hPutStrLn stderr $ "** sending submission request for '" ++ pslug ++ "'"
      doSubmit json
  where
    doSubmit json = do
      createDirectoryIfMissing True transactionDir
      out <- readProcess "./api/submit.sh" [] json
      ts  <- getZonedTime
      writeFile (outPath ts) out
    outPath ts = transactionDir </> (pslug ++ formatTime defaultTimeLocale "_%d-%H%M%S_out" ts) <.> "json"
    getSubmit = do
      source <- readFile (solutionDir </> pslug <.> "man")
      pure $ L8.unpack $ encode (Submit (pstatId ps) source)
    pslug = pstatSlug ps


submit :: String -> IO ()
submit key = do
  ps <- loadProbStatList
  let lookup' = lookupProbStat pstatSlug key ps <|> lookupProbStat pstatName key ps
  case lookup' of
    Nothing  -> fail $ "problem not found for: " ++ key
    Just p   -> submit' p
