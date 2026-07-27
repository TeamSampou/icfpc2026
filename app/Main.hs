module Main where

import Lib
import Exam

main :: IO ()
main = do
  -- putStr $ enc'' history
  putStr . unlines $ stack2Layout 100
