module Main where

import Lib
import Exam

main :: IO ()
main = do
  putStr . unlines $ stack4Layout 100
