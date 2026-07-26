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
pp1ProbStat' = printf "%-25s%-20s%-9s%s"

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

pp1Problem' :: String -> String -> String -> String -> String -> String -> String
pp1Problem' = printf "%-25s%-16s%-9s%-7s%-13s%s"

type DescSize = Maybe Int

pp1Problem :: DescSize -> Problem -> String
pp1Problem maySZ p = pp1Problem' (probSlug p) (probScoring p) tickCap ustrict setName desc
  where
    _name = q (probName p)
    tickCap = maybe "null" show (probTickCap p)
    ustrict = if probUberStrict p then "true" else "false"
    setName = case probProblemSetName p of
      "Practice Problems (Ungraded)"  -> "'(Ungraded)'"
      sn                              -> q sn
    q s = "'" ++ s ++ "'"
    desc
      | Just sz <- maySZ  = take sz $ intercalate " " $ lines $ probDescription p
      | otherwise         = ""

viewProblems' :: DescSize -> IO ()
viewProblems' maySZ = do
  ps <- loadProblemList
  let desch
        | Just {} <- maySZ  = "description"
        | otherwise         = ""
      hd = pp1Problem' "slug" "scoring" "tick-cap" "strict" "problem-set" desch
      xs = map (pp1Problem maySZ) $ sortOn probProblemSetName ps
  mapM_ putStrLn $ hd : "" : xs

viewProblems :: IO ()
viewProblems = viewProblems' Nothing

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
