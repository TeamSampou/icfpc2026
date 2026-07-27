module Main 
    ( main
    )
    where

import Data.Char ( ord, intToDigit )
import Data.List.Split ( chunksOf )
import Exam ( history )

main :: IO ()
main = putStr mkLesson
        
cn :: Int
cn = 105

list :: b -> (a -> [a] -> b) -> [a] -> b
list z f xs = case xs of
    []   -> z
    y:ys -> f y ys

conv :: Char -> (Int,Int)
conv c = (ord c - 32) `divMod` 10

send :: Char -> String
send c = case conv c of
    (i,j) | i == j    -> intToDigit i : "SS"
          | otherwise -> intToDigit i : 'S' : intToDigit j : "S"

hists :: [String]
hists = list [] phi 
      $ chunksOf cn $ concatMap send history
    where
        phi xs xss = ("|@"++xs++"v|") : zipWith id (cycle [f,g]) xss
        f xs = "|v"++reverse (take cn (xs ++ repeat ' '))++"<|"
        g xs = "|>"++         take cn (xs ++ repeat ' ') ++"v|"

mkLesson :: String
mkLesson = unlines
         $  [ubline]
         ++  hists
         ++ [ubline]
         ++ ["^         +---------------+v"
            ,"^<+-++-+  |>@RW`10`*W`32`v|<"
            ,"  |I||O|<<|^S+R`  `W+`  `<|"
            ,"  +-++-+  +---------------+"
            ]
    where
        ubline = "+"++replicate (cn+2) '-'++"+"
