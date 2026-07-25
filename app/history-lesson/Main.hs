module Main
    ( main
    ) where

import System.FilePath

loadInt :: Int -> String
loadInt x
    | x < 0     = undefined
    | x < 10    = show x
    | otherwise = "`" ++ show x ++ "`"

main :: IO ()
main = do
    s <- readFile rfp
    let cmds = "@" ++ concat [loadInt (fromEnum c) ++ "S" | c <- s] ++ " "
    writeFile wfp $ unlines $
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
    where
        rfp = "solutions" </> "txt" </> "history-lesson.txt"
        wfp = "solutions" </> "man" </> "history-lesson.man"
    