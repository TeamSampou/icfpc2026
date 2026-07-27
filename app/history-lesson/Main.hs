module Main 
    ( main
    )
    where

import Data.Bits
import Data.Char
import Data.List
import Data.Ord
import Data.List.Split
import Text.Printf
import Exam ( history )

main :: IO ()
main = do
    putStr mkHistory
    -- print $ length $ encs history
    -- mapM_ print $ encs history

cn :: Int
cn = 22

encs :: String -> [String]
encs = list [] psi . map concat . chunksOf cn . ("`32` W" :) . map phi
    where
        phi :: Char -> String
        phi c   = printf "`%02d`+S" (ord c - 32)
        psi :: String -> [String] -> [String]
        psi xs xss = ("|@ "++init xs++"v|") : zipWith id (cycle [f,g]) xss
                where
                    f s | length s == cn * 6 = "|v" ++ drop 1 (reverse s) ++ "S<|"
                        | otherwise          = "|v" ++  replicate (cn*6 - length s - 1) ' '  ++ 'S' : drop 1 (reverse s) ++ "S<|"
                    g s | length s == cn * 6 = "|>S" ++ init s ++ "v|"
                        | otherwise          = "|>S" ++ init s ++ 'S' : replicate (cn*6 - length s - 1) ' ' ++  "v|"
        
list :: b -> (a -> [a] -> b) -> [a] -> b
list z f xs = case xs of
    []   -> z
    y:ys -> f y ys

mkHistory :: String
mkHistory 
    = unlines
    $  [tbline]
    ++ encs history
    ++ [tbline]
    ++ [" ^  v"," ^  v","+-++-+","|I||O|","+-++-+"]
    where
        tbline = "+" ++ replicate (6*cn+2) '-' ++ "+"
