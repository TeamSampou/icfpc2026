import System.Environment

import FilesJSON

usage :: IO ()
usage = do
  putStr $ unlines
    [ "submit {SLUG_NAME|PROBLEM_NAME} [MAN_FILEPATH]"
    , ""
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      usage
      fail "SLUG_NAME or PROBLEM_NAME is required!"
    [a]    -> submit a Nothing
    a:b:_  -> submit a (Just b)
