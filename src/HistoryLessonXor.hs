{-# OPTIONS -Wno-x-partial #-}

module HistoryLessonXor where

import Data.Bits
import Data.List
import System.FilePath
import Text.Printf

rpath :: FilePath
rpath = "solutions" </> "txt" </> "history-lesson" <.> "txt"

loadInt :: Int -> IO String
loadInt x
    | x < 0     = fail $ "loadInt: not negative required: " ++ show x
    | x < 10    = pure $ show x
    | otherwise = pure $ "`" ++ show x ++ "`"

------------------------------------------------------------------------

-- 縦方向の ` ` 判定を回避する方法を考える必要あり
genRectCmd :: [Cmd] -> [String]
genRectCmd cmds = case genRectBlock cmds of
  []        -> error "genRectCmd: null input"
  xs@(x:_)  ->
    [ "+" ++ wall ++ "+"]               ++
    [ "|" ++ cstr ++ "|" | cstr <- xs]  ++
    [ "+" ++ wall ++ "+"
    , pad ++ "^<+-++-+v"
    , pad ++ "  |I||O|<"
    , pad ++ "  +-++-+"
    ]
    where
      wall = replicate wlen '-'
      wlen = length x
      --
      pad = replicate plen ' '
      plen = wlen + 2 - iow
      iow = length "^<+-++-+v"

genRectBlock :: [Cmd] -> [String]
genRectBlock cmds = blocks
  where
    blocks =
      frBlock '@' hd                        ++
      [s | md <- mds, s <- frBlock '>' md]  ++
      frLast '>' lst

    hd   = head frs0
    mds  = init tl0
    lst  = last tl0
    tl0  = tail frs0

    frBlock fc (flen, fwd, rlen, rev) =
      [ [fc] ++ fwd ++ replicate (fillw - flen) ' ' ++ "v"
      , "v"  ++ replicate (fillw - rlen) ' ' ++ rev ++ "<"
      ]

    frLast fc (_flen, fwd, _rlen, []) =
      [ [fc] ++ fwd ]
    frLast fc ( flen, fwd,  rlen, rev) =
      [ [fc] ++ fwd ++ replicate (fillw - flen) ' ' ++ "v"
      , replicate (fillw - rlen) ' ' ++ "H"  ++ rev ++ "<"
      ]

    fillw = maximum [len | (flen, _fwd, rlen, _rev) <- frs0, len <- [flen, rlen]]

    {- concat (revert $ map reverse rev) === reverse $ concat rev -}
    frs0 = [ (length fwd, fwd, length rev, rev)
           | (f0, r0) <- pairChunks cks
           , let fwd = concat f0
                 rev = reverse $ concat r0
           ]
    cks = unfoldr (takeChunksTh threshold length) cmds
    {-    |----- x -----|
        "@...............v"   ---
        "v...............<"    |
        ">...............v" (x - 1)
        "v...............<"    |
                 .....        ---

                    I  O   3×3  -}
    {- (x + 2) * (x - 1) ≥ total  ⇔  x^2 + x - 2 - total ≥ 0
       D = 1^2 + 4 * (2 + total) ,  α ≥ (-1 + √D) / 2
     -}
    threshold = ceiling $ ( (- 1) + sqrt (1 + 4 * (2 + fromIntegral total)) ) / (2  :: Double)
    total = sum $ map length cmds

pairChunks :: [[a]] -> [([a], [a])]
pairChunks []        =            []
pairChunks [x]       = ( x, []) : []
pairChunks (x:y:xs)  = ( x,  y) : pairChunks xs

takeChunksTh :: Int -> (a -> Int) -> [a] -> Maybe ([a], [a])
takeChunksTh _th _length' []  = Nothing
takeChunksTh  th  length' css0 = go 0 id css0
  where
    go clen ac ccs
      | clen < th  = case ccs of
          []       -> Just (ac [], [])
          c:cs     -> go (clen + length' c) (ac . (c:)) cs
      | otherwise   = Just (ac [], ccs)

------------------------------------------------------------------------

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

genOneLineCmd :: String -> [String]
genOneLineCmd cmds =
  [ "+" ++ wall ++ "+"
  , "|" ++ cmds ++ "|"
  , "+" ++ wall ++ "+"
  , pad ++ "^<+-++-+v"
  , pad ++ "  |I||O|<"
  , pad ++ "  +-++-+"
  ]
  where
    wall = replicate len '-'
    len = length cmds
    --
    pad = replicate plen ' '
    plen = len + 2 - iow
    iow = length "^<+-++-+v"

------------------------------------------------------------------------

writeCmdFile :: FilePath -> [String] -> IO ()
writeCmdFile wpath xs = writeFile wpath $ unlines xs

------------------------------------------------------------------------

type Cmd = String

getXorCmds :: Int -> IO [Cmd]
getXorCmds k = do
  s  <- readFile rpath
  let xs = map (xor k) $ map fromEnum s
  ck <- loadInt k
  sxs  <- mapM loadInt xs
  let cmds = ck : "W" : [c | sx <- sxs, c <- [sx, "~", "S"]]
  pure cmds

------------------------------------------------------------------------

writeXorsRect :: Int -> IO ()
writeXorsRect k = do
  cmds <- getXorCmds k
  let wpath = ("solutions" </> "man" </> "history-lesson-" ++ show k <.> "man")
  writeCmdFile wpath $ genRectCmd cmds

writeXorsRect96 :: IO ()
writeXorsRect96 = writeXorsRect 96

------------------------------------------------------------------------

-- for one-line cmds
writeXorsOneLine :: Int -> IO ()
writeXorsOneLine k = do
  cs <- concat . ("@" :) <$> getXorCmds k
  let wpath = ("solutions" </> "man" </> "history-lesson-l" ++ show k <.> "man")
  writeCmdFile wpath $ genOneLineCmd cs

writeXorsOneLine96 :: IO ()
writeXorsOneLine96 = writeXorsOneLine 96

writeXorsOneLine64 :: IO ()
writeXorsOneLine64 = writeXorsOneLine 64

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
  let wpath = ("solutions" </> "man" </> "history-lesson-ajdxor" <.> "man")
  writeCmdFile wpath $ genOneLineCmd cmds

------------------------------------------------------------------------
