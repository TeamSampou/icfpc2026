module Main where

import Lib
import Exam

main :: IO ()
main = putStr . unlines $ makeLayout [0..99]
