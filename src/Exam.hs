module Exam where

import Data.Map (Map)
import Data.Set (Set)

-- import Prelude hiding (div, subtract)
-- 
-- -- | 一旦 R や S だけ考える。近い遠いは気にせず拾えた順に第1引数第2引数として R で処理する
-- data Instr = Lit Integer -- 0-9
--            | Add   -- ^ + : regA := regA + regB
--            | Mul   -- ^ * : regA := regA * regB
--            | Sub   -- ^ - : regA := regA - regB
--            | Div   -- ^ % : regA := regA % regB, regB := reminder of (regA / regB)
--            | Swap  -- ^ W : regA := regB, regB := regA
--            | Copy  -- ^ M : regB := regA
--            | Receive -- ^ R
--            | Send    -- ^ S
--            deriving (Ord, Eq)
-- 
-- instance Show Instr where
--   show (Lit n) = show n
--   show Add     = "+"
--   show Mul     = "*"
--   show Sub     = "-"
--   show Div     = "/"
--   show Swap    = "W"
--   show Copy    = "M"
--   show Receive = "R"
--   show Send    = "S"
-- 
-- 
-- genInstrsCommutable :: Instr -> Integer -> [Instr]
-- genInstrsCommutable op n
--   = [ Receive -- ^regA : x, regB : ?
--     , Copy    -- ^regA : x, regB : x
--     , Lit n   -- ^regA : n, regB : x
--     , op      -- ^regA : n `op` x, regB : x
--     , Send    -- ^n `op` x
--     ]
-- 
-- -- | \x -> x + n
-- -- commutable
-- add :: Integer -> [Instr]
-- add = genInstrsCommutable Add
-- 
-- -- | \x -> x * n
-- -- commutable
-- mul :: Integer -> [Instr]
-- mul = genInstrsCommutable Mul
-- 
-- genInstrsIncommutable :: Instr -> Integer -> [Instr]
-- genInstrsIncommutable op n
--   = [ Receive -- ^regA : x, regB : ?
--     , Copy    -- ^regA : x, regB : x
--     , Lit n   -- ^regA : n, regB : x
--     , Swap    -- ^regA : x, regB : n
--     , op      -- ^regA : x `op` n, regB : n
--     , Send    -- ^x `op` n
--     ]
-- 
-- -- | \x -> x - n
-- sub :: Integer -> [Instr]
-- sub = genInstrsIncommutable Sub
-- 
-- -- | \x -> x / n
-- div :: Integer -> [Instr]
-- div = genInstrsIncommutable Div
-- 
-- genInstrsCommutableBin :: Instr -> [Instr]
-- genInstrsCommutableBin op
--   = [ Receive -- ^regA : x, regB : ?
--     , Copy    -- ^regA : x, regB : x
--     , Receive -- ^regA : y, regB : x
--     , op      -- ^regA : y `op` x, regB : x
--     , Send    -- ^y `op` x
--     ]
-- 
-- plus :: [Instr]
-- plus = genInstrsCommutableBin Add
-- 
-- times :: [Instr]
-- times = genInstrsCommutableBin Mul
-- 
-- genInstrsIncommutableBin :: Instr -> [Instr]
-- genInstrsIncommutableBin op
--   = [ Receive -- ^regA : x, regB : ?
--     , Copy    -- ^regA : x, regB : x
--     , Receive -- ^regA : y, regB : x
--     , Swap    -- ^regA : x, regB : y
--     , op      -- ^regA : x `op` y, regB : y
--     , Send    -- ^x `op` y
--     ]
-- 
-- subtract :: [Instr]
-- subtract = genInstrsIncommutableBin Sub
-- 
-- divide :: [Instr]
-- divide = genInstrsIncommutableBin Div
-- 
-- commutable :: Instr -> Bool
-- commutable Add = True
-- commutable Mul = True
-- commutable Sub = False
-- commutable Div = False
-- commutable op  = error $ "commutable can't judge for: " ++ show op
-- 
-- -- S combinator
-- -- S f g x = f x (g x)
-- starling :: [Instr] -> [Instr] -> Maybe [Instr]
-- -- op1 and op2 are commutable
-- starling [Receive, Copy, Receive, op2, Send] [Receive, Copy, Lit n, op1, Send]
--   = Just [ Receive  -- ^regA : x, regB : ?
--          , Copy     -- ^regA : x, regB : x
--          , Lit n    -- ^regA : n, regB : x
--          , op1      -- ^regA : n op1 x, regB : x
--          , op2      -- ^regA : (n op1 x) op2 x, regB : x
--          , Send     -- ^(n op1 x) op2 x
--          ]
-- -- only op1 is commutable
-- starling [Receive, Copy, Receive, Swap, op2, Send] [Receive, Copy, Lit n, op1, Send]
--   = Just [ Receive  -- ^regA : x, regB : ?
--          , Copy     -- ^regA : x, regB : x
--          , Lit n    -- ^regA : n, regB : x
--          , op1      -- ^regA : n op1 x, regB : x
--          , Swap     -- ^regA : x, regB : n op1 x
--          , op2      -- ^regA : x op2 (n op1 x), regB : n op1 x
--          , Send     -- ^x op2 (n op1 x)
--          ]
-- starling _ _ = Nothing
-- 
-- -- B combinator
-- -- B f g x = f (g x)
-- kestrel :: [Instr] -> [Instr] -> Maybe [Instr]
-- -- op1 and op2 are commutable
-- kestrel f g = go [] (g ++ f)
--   where go acc [] = Just (reverse acc)
--         go acc (op:Send:Receive:Copy:is)
--           | commutable op = go (Copy:op:acc) is
--           | otherwise    = Nothing
--         go acc (op:ops) = go (op:acc) ops
-- 
-- triangle :: Maybe [Instr]
-- triangle = do
--   p <- starling times (add 1)
--   kestrel (div 2) p
-- 
-- main :: IO ()
-- main = case triangle of
--   Just instrs -> putStrLn $ "@" ++ concatMap show instrs ++ "H"
--   Nothing     -> putStrLn "Failed to generate instructions."

fanout :: Int -> [String]
fanout height = [ "+--+"
                , "|@v|"
                , "|>v|"
                , "|.R|"
                , "|.S|"
                , "|^<|"
                ]
                ++ replicate h "|  |" ++
                ["+--+"]
  where h = height - 7

vConnect :: Int -> [Int] -> [String]
vConnect height ns = map line [1..height]
  where line i = if i `elem` ns then ">>" else "  "

judge :: Int -> [String]
judge n = [ "+-------+"
          , "|>  @rbv|"
          , "|^ v < r|"
          , "|   vX<M|"
          , "|^ v < `|"
          , "| vdav " ++ show d2 ++ "|"
          , "| r 01 " ++ show d1 ++ "|"
          , "|^<< s " ++ show d0 ++ "|"
          , "|^   r-`|"
          , "|^ S<<^<|"
          , "+-------+"
          ]
  where  (d2, r) = divMod n 100
         (d1, d0) = divMod r 10

keepValue :: [String]
keepValue = [ "+-----+  "
            , "|>@rbv|  "
            , "| vWrd|  "
            , "|^<  W|  "
            , "|^ Ws<|  "
            , "+-----+  "
            ]

header :: Int -> [String]
header n = [ "+--------"
           , "|@>Rsv   "
           , "|.^..<   "
           , "+--------"
           , "^        "
           , "^        "
           ]
           ++ keepValue ++
           [ "^        "
           , "^        "
           ]
           ++ judge n ++
           [ "^        "
           , "^        "
           , "+--------"
           , "|>@Rv    "
           , "|^ S<    "
           , "+--------"
           , "v        "
           , "v        "
           ]
           ++ judge (n+1) ++
           [ "v        "
           , "v        "
           ]
           ++ keepValue ++
           [ " v       "
           , " v       "
           , "+--------"
           , "|@>Rsv   "
           , "|.^..<   "
           , "+--------"
           ]
  
column :: Int -> [String]
column n = [ "---------"
           , "         "
           , "         "
           , "---------"
           , "^        "
           , "^        "
           ]
           ++ keepValue ++
           [ "^        "
           , "^        "
           ]
           ++ judge n ++
           [ "^        "
           , "^        "
           , "---------"
           , "         "
           , "         "
           , "---------"
           , "v        "
           , "v        "
           ]
           ++ judge (n+1) ++
           [ "v        "
           , "v        "
           ]
           ++ keepValue ++
           [ " v       "
           , " v       "
           , "---------"
           , "         "
           , "         "
           , "---------"
           ]
  

tailer :: Int -> [String]
tailer n = [ "--------+"
           , "        |"
           , "        |"
           , "--------+"
           , "^        "
           , "^        "
           ]
           ++ keepValue ++
           [ "^        "
           , "^        "
           ]
           ++ judge n ++
           [ "^        "
           , "^        "
           , "--------+"
           , "        |"
           , "        |"
           , "--------+"
           , "v        "
           , "v        "
           ]
           ++ judge (n+1) ++
           [ "v        "
           , "v        "
           ]
           ++ keepValue ++
           [ " v       "
           , " v       "
           , "--------+"
           , "        |"
           , "        |"
           , "--------+"
           ]

body :: [Int] -> [String]
body = hcat . map column

hcat :: [[String]] -> [String]
hcat = foldr1 (zipWith (++))
       
makeLayout :: [Int] -> [String]
makeLayout xs = hcat [header s, body ns, tailer e]
  where s = head xs
        e = last xs
        ns = tail (init xs)

stack2Layout :: Int -> [String]
stack2Layout size =  hcat [ fanout height
                          , vConnect height [cH, hEven + cH]
                          , evens ++ odds
                          , vConnect height [1, hEven, hEven + 1, hEven + hOdd]
                          , fanout height
                          ]
  where evens = makeLayout [0,4..size-1]
        odds  = makeLayout [2,6..size-1]
        hEven = length evens
        cH    = hEven `div` 2
        hOdd  = length odds
        height = hEven + hOdd

stack4Layout :: Int -> [String]
stack4Layout size =  hcat [ fanout height
                          , vConnect height $ scanl (+) 1 [hq0, hq1, hq2]
                          , concat [q0, q1, q2, q3]
                          , vConnect height $ scanl (+) hq0 [hq1, hq2, hq3]
                          , fanout height
                          ]
  where q0 = makeLayout [0,4..size-1]
        q1 = makeLayout [1,5..size-1]
        q2 = makeLayout [2,6..size-1]
        q3 = makeLayout [3,7..size-1]
        hq0 = length q0
        hq1 = length q1
        hq2 = length q2
        hq3 = length q3
        height = sum [hq0, hq1, hq2, hq3]
