loadInt :: Int -> String
loadInt x
  | x < 0     = undefined
  | x < 10    = show x
  | otherwise = "`" ++ show x ++ "`"

main :: IO ()
main = do
  s <- readFile("icfp-history.txt")
  let cmds = "@" ++ concat [loadInt (fromEnum c) ++ "S" | c <- s] ++ " "
  writeFile "history.man" $ unlines $
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
