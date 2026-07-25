module HistoryLessonXor where

import Data.Bits
import System.FilePath
import Text.Printf

rpath :: FilePath
rpath = "solutions" </> "txt" </> "history-lesson" <.> "txt"

loadInt :: Int -> IO String
loadInt x
    | x < 0     = fail $ "loadInt: not negative required: " ++ show x
    | x < 10    = pure $ show x
    | otherwise = pure $ "`" ++ show x ++ "`"

writeOneLineCmd :: FilePath -> String -> IO ()
writeOneLineCmd wpath cmds = do
  writeFile wpath $ unlines $
    [ "+" ++ replicate (length cmds) '-' ++ "+"
    , "|" ++ cmds ++ "|"
    , "+" ++ replicate (length cmds) '-' ++ "+"
    , " ^  v "
    , " |  | "
    , " ^  v "
    , "+-++-+"
    , "|I||O|"
    , "+-++-+"
    ]

------------------------------------------------------------------------

bitCounts :: IO ()
bitCounts = do
  s  <- readFile rpath
  let ints = map fromEnum s
  putStr $ unlines $
    ("all: " ++ show (length ints)) :
    [printf "%3d: " (2^b :: Int) ++ show (length $ filter (`testBit` b) ints)
    | b <- [6,5..0]
    ]

xorsK :: (Num a, Bits a) => a -> [a] -> [a]
xorsK k = map (xor k)

getXorsK :: Int -> IO String
getXorsK k = do
  s  <- readFile rpath
  let xs = xorsK k $ map fromEnum s
  sk <- loadInt k
  sxs  <- mapM loadInt xs
  let cmds = "@" ++ sk ++  "W" ++ [c | sx <- sxs, c <- sx ++ "~S"] ++ " "
  pure cmds

writeXorsK :: Int -> IO ()
writeXorsK k = do
  cmds <- getXorsK k
  writeOneLineCmd ("solutions" </> "man" </> "history-lesson" <.> "man") cmds

writeXors96 :: IO ()
writeXors96 = writeXorsK 96

writeXors64 :: IO ()
writeXors64 = writeXorsK 64

------------------------------------------------------------------------

adjacentXor :: Bits a => [a] -> Maybe (a, [a])
adjacentXor      []     = Nothing
adjacentXor xxs@(x:xs)  = Just (x, zipWith xor xxs xs)

getAdjacentXor :: IO String
getAdjacentXor = do
  s  <- readFile rpath
  (i, xs) <- maybe (fail "adjacentXor: requires not empty") pure  $ adjacentXor $ map fromEnum s
  si   <- loadInt i
  sxs  <- mapM loadInt xs
  let cmds = "@" ++ si ++ "SW" ++ [c | sx <- sxs, c <- sx ++ "~SW"] ++ " "
  pure cmds

writeAdjacentXor :: IO ()
writeAdjacentXor = do
  cmds <- getAdjacentXor
  writeOneLineCmd ("solutions" </> "man" </> "history-lesson" <.> "man") cmds

------------------------------------------------------------------------
