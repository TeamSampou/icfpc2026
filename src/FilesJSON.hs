{-# OPTIONS -Wno-missing-export-lists #-}

module FilesJSON where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (toLower)
import Data.List
import Data.Time
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)
import Text.Printf

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

pp1ProbStat' :: String -> String -> String -> String -> String
pp1ProbStat' = printf "%-16s%-20s%-9s%s"

pp1ProbStat :: ProbStat -> String
pp1ProbStat p =
    pp1ProbStat' (pstatSlug p) name (pstatStatus p) setName
  where
    name = q (pstatName p)
    setName = q (pstatProblemSetName p)
    q s = "'" ++ s ++ "'"

searchNames :: String -> IO ()
searchNames w = do
  ps <- loadProbStatList
  let hd = pp1ProbStat' "slug" "name" "status" "problem-set"
      xs = map pp1ProbStat $ sortOn pstatProblemSetName $ filter search ps
  mapM_ putStrLn $ hd : "" : xs
  where
    search p = w `isInfixOf` pstatSlug p || w `isInfixOf` lower (pstatName p)
    lower = map toLower

pp1Problem' :: String -> String -> String -> String -> String
pp1Problem' = printf "%-16s%-16s%-13s%s"

pp1Problem :: Problem -> String
pp1Problem p = pp1Problem' (probSlug p) (probScoring p) setName desc
  where
    name = q (probName p)
    setName = case probProblemSetName p of
      "Practice Problems (Ungraded)"  -> "'(Ungraded)'"
      sn                              -> q sn
    q s = "'" ++ s ++ "'"
    desc = take 40 $ intercalate " " $ lines $ probDescription p

viewProblems :: IO ()
viewProblems = do
  ps <- loadProblemList
  let hd = pp1Problem' "slug" "scoring" "problem-set" "description"
      xs = map pp1Problem $ sortOn probProblemSetName ps
  mapM_ putStrLn $ hd : "" : xs

submit' :: ProbStat -> Maybe String -> IO ()
submit' ps mayFn = do
      hPutStrLn stderr $ "** generating submission request for '" ++ tag ++ "'"
      json <- getSubmit
      hPutStrLn stderr $ "** sending submission request for '" ++ tag ++ "'"
      doSubmit json
  where
    doSubmit json = do
      createDirectoryIfMissing True transactionDir
      out <- readProcess "./api/submit.sh" [] json
      ts  <- getZonedTime
      writeFile (outPath ts) out
    outPath ts = transactionDir </> (pslug ++ formatTime defaultTimeLocale "_%d-%H%M%S_out" ts) <.> "json"
    getSubmit = do
      source <- readFile (solutionDir </> maybe pslug id mayFn <.> "man")
      pure $ L8.unpack $ encode (Submit (pstatId ps) source)
    tag = maybe pslug (\m -> pslug ++ "(" ++ m ++ ")") mayFn
    pslug = pstatSlug ps


submit :: String -> Maybe String -> IO ()
submit key mayFn = do
  ps <- loadProbStatList
  let lookupPS f = find ((== key) . f) ps
  case lookupPS pstatSlug <|> lookupPS pstatName of
    Nothing  -> fail $ "problem not found for: " ++ key
    Just p   -> submit' p mayFn
