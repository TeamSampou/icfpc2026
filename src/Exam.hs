module Exam where

import Data.Char (ord)
import Data.Map (Map)
import Data.Set (Set)
import Data.Bits (xor)

{-
-- | 一旦 R や S だけ考える。近い遠いは気にせず拾えた順に第1引数第2引数として R で処理する
import Prelude hiding (div, subtract)

data Instr = Lit Integer -- 0-9
           | Add   -- ^ + : regA := regA + regB
           | Mul   -- ^ * : regA := regA * regB
           | Sub   -- ^ - : regA := regA - regB
           | Div   -- ^ % : regA := regA % regB, regB := reminder of (regA / regB)
           | Swap  -- ^ W : regA := regB, regB := regA
           | Copy  -- ^ M : regB := regA
           | Receive -- ^ R
           | Send    -- ^ S
           deriving (Ord, Eq)

instance Show Instr where
  show (Lit n) = show n
  show Add     = "+"
  show Mul     = "*"
  show Sub     = "-"
  show Div     = "/"
  show Swap    = "W"
  show Copy    = "M"
  show Receive = "R"
  show Send    = "S"


genInstrsCommutable :: Instr -> Integer -> [Instr]
genInstrsCommutable op n
  = [ Receive -- ^regA : x, regB : ?
    , Copy    -- ^regA : x, regB : x
    , Lit n   -- ^regA : n, regB : x
    , op      -- ^regA : n `op` x, regB : x
    , Send    -- ^n `op` x
    ]

-- | \x -> x + n
-- commutable
add :: Integer -> [Instr]
add = genInstrsCommutable Add

-- | \x -> x * n
-- commutable
mul :: Integer -> [Instr]
mul = genInstrsCommutable Mul

genInstrsIncommutable :: Instr -> Integer -> [Instr]
genInstrsIncommutable op n
  = [ Receive -- ^regA : x, regB : ?
    , Copy    -- ^regA : x, regB : x
    , Lit n   -- ^regA : n, regB : x
    , Swap    -- ^regA : x, regB : n
    , op      -- ^regA : x `op` n, regB : n
    , Send    -- ^x `op` n
    ]

-- | \x -> x - n
sub :: Integer -> [Instr]
sub = genInstrsIncommutable Sub

-- | \x -> x / n
div :: Integer -> [Instr]
div = genInstrsIncommutable Div

genInstrsCommutableBin :: Instr -> [Instr]
genInstrsCommutableBin op
  = [ Receive -- ^regA : x, regB : ?
    , Copy    -- ^regA : x, regB : x
    , Receive -- ^regA : y, regB : x
    , op      -- ^regA : y `op` x, regB : x
    , Send    -- ^y `op` x
    ]

plus :: [Instr]
plus = genInstrsCommutableBin Add

times :: [Instr]
times = genInstrsCommutableBin Mul

genInstrsIncommutableBin :: Instr -> [Instr]
genInstrsIncommutableBin op
  = [ Receive -- ^regA : x, regB : ?
    , Copy    -- ^regA : x, regB : x
    , Receive -- ^regA : y, regB : x
    , Swap    -- ^regA : x, regB : y
    , op      -- ^regA : x `op` y, regB : y
    , Send    -- ^x `op` y
    ]

subtract :: [Instr]
subtract = genInstrsIncommutableBin Sub

divide :: [Instr]
divide = genInstrsIncommutableBin Div

commutable :: Instr -> Bool
commutable Add = True
commutable Mul = True
commutable Sub = False
commutable Div = False
commutable op  = error $ "commutable can't judge for: " ++ show op

-- S combinator
-- S f g x = f x (g x)
starling :: [Instr] -> [Instr] -> Maybe [Instr]
-- op1 and op2 are commutable
starling [Receive, Copy, Receive, op2, Send] [Receive, Copy, Lit n, op1, Send]
  = Just [ Receive  -- ^regA : x, regB : ?
         , Copy     -- ^regA : x, regB : x
         , Lit n    -- ^regA : n, regB : x
         , op1      -- ^regA : n op1 x, regB : x
         , op2      -- ^regA : (n op1 x) op2 x, regB : x
         , Send     -- ^(n op1 x) op2 x
         ]
-- only op1 is commutable
starling [Receive, Copy, Receive, Swap, op2, Send] [Receive, Copy, Lit n, op1, Send]
  = Just [ Receive  -- ^regA : x, regB : ?
         , Copy     -- ^regA : x, regB : x
         , Lit n    -- ^regA : n, regB : x
         , op1      -- ^regA : n op1 x, regB : x
         , Swap     -- ^regA : x, regB : n op1 x
         , op2      -- ^regA : x op2 (n op1 x), regB : n op1 x
         , Send     -- ^x op2 (n op1 x)
         ]
starling _ _ = Nothing

-- B combinator
-- B f g x = f (g x)
kestrel :: [Instr] -> [Instr] -> Maybe [Instr]
-- op1 and op2 are commutable
kestrel f g = go [] (g ++ f)
  where go acc [] = Just (reverse acc)
        go acc (op:Send:Receive:Copy:is)
          | commutable op = go (Copy:op:acc) is
          | otherwise    = Nothing
        go acc (op:ops) = go (op:acc) ops

triangle :: Maybe [Instr]
triangle = do
  p <- starling times (add 1)
  kestrel (div 2) p

main :: IO ()
main = case triangle of
  Just instrs -> putStrLn $ "@" ++ concatMap show instrs ++ "H"
  Nothing     -> putStrLn "Failed to generate instructions."
-}


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
          , "|^   s " ++ show d0 ++ "|"
          , "|^ < r~`|"
          , "|^<S<<^<|"
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
           , "^<+-----+"
           , "  |>@rbv|"
           , "  | vWrd|"
           , "  |^<  W|"
           , " >|^ Ws<|"
           , " ^+-----+"
           ]
           ++ judge n ++
           [ "^        "
           , "^        "
           , "+--------"
           , "|@>RSv   "
           , "| ^  <   "
           , "+--------"
           , "v        "
           , "v        "
           ]
           ++ judge (n+1) ++
           [ " v+-----+"
           , " >|>@rbv|"
           , "  | vWrd|"
           , "  |^<  W|"
           , "  |^ Ws<|"
           , "v<+-----+"
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
           , "+-----+>^"
           , "|>@rbv|  "
           , "| vWrd|  "
           , "|^<  W|  "
           , "|^ Ws<|< "
           , "+-----+^ "
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
           [ "+-----+v "
           , "|>@rbv|< "
           , "| vWrd|  "
           , "|^<  W|  "
           , "|^ Ws<|  "
           , "+-----+>v"
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
           , "+-----+>^"
           , "|>@rbv|  "
           , "| vWrd|  "
           , "|^<  W|  "
           , "|^ Ws<|< "
           , "+-----+^ "
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
           [ "+-----+v "
           , "|>@rbv|< "
           , "| vWrd|  "
           , "|^<  W|  "
           , "|^ Ws<|  "
           , "+-----+>v"
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

-- head, column, tailer で 3 列以上つまり 4 * 3 = 12 以上のサイズが必要になる。
-- stack2 は 2 rows の制御バスを持つ。制御バスの上下にメモリマットを配置するので縦に4セル持つ。
stack2Layout :: Int -> [String]
stack2Layout size =  hcat [ fanout height
                          , vConnect height $ scanl (+) cH [hEven]
                          , evens ++ odds
                          , vConnect height $ scanl (+) 1 [hEven] ++ scanl (+) hEven [hOdd]
                          , fanout height
                          ]
  where evens = makeLayout [0,4..size-1]
        odds  = makeLayout [2,6..size-1]
        hEven = length evens
        hOdd  = length odds
        cH    = hEven `div` 2
        height = hEven + hOdd

stack3Layout :: Int -> [String]
stack3Layout size =  hcat [ fanout height
                          , vConnect height $ scanl (+) cH [hq0, hq1]
                          , concat [q0, q1, q2]
                          , vConnect height $ scanl (+) 1 [hq0, hq1] ++ scanl (+) hq0 [hq1, hq2]
                          , fanout height
                          ]
  where q0 = makeLayout [0,6..size-1]
        q1 = makeLayout [2,8..size-1]
        q2 = makeLayout [4,10..size-1]
        hq0 = length q0
        hq1 = length q1
        hq2 = length q2
        cH  = hq0 `div` 2
        height = hq0 + hq1 + hq2

-- head, column, tailer で 3 列以上つまり 8 * 3 = 24 以上のサイズが必要になる。
-- stack4 は 4 rows の制御バスを持つ。制御バスの上下にメモリマットを配置するので縦に8セル持つ。
stack4Layout :: Int -> [String]
stack4Layout size =  hcat [ fanout height
                          , vConnect height $ scanl (+) cH [hq0, hq1, hq2]
                          , concat [q0, q1, q2, q3]
                          , vConnect height $ scanl (+) 1 [hq0, hq1, hq2] ++ scanl (+) hq0 [hq1, hq2, hq3]
                          , fanout height
                          ]
  where q0 = makeLayout [0,8..size-1]
        q1 = makeLayout [2,10..size-1]
        q2 = makeLayout [4,12..size-1]
        q3 = makeLayout [6,14..size-1]
        hq0 = length q0
        hq1 = length q1
        hq2 = length q2
        hq3 = length q3
        cH  = hq0 `div` 2
        height = hq0 + hq1 + hq2 + hq3

-- head, column, tailer で 3 列以上つまり 10 * 3 = 30 以上のサイズが必要になる。
-- stack5 は 5 rows の制御バスを持つ。制御バスの上下にメモリマットを配置するので縦に10セル持つ。
stack5Layout :: Int -> [String]
stack5Layout size =  hcat [ fanout height
                          , vConnect height $ scanl (+) cH [hq0, hq1, hq2, hq3]
                          , concat [q0, q1, q2, q3, q4]
                          , vConnect height $ scanl (+) 1 [hq0, hq1, hq2, hq3] ++ scanl (+) hq0 [hq1, hq2, hq3, hq4]
                          , fanout height
                          ]
  where q0 = makeLayout [0,10..size-1]
        q1 = makeLayout [2,12..size-1]
        q2 = makeLayout [4,14..size-1]
        q3 = makeLayout [6,16..size-1]
        q4 = makeLayout [8,18..size-1]
        hq0 = length q0
        hq1 = length q1
        hq2 = length q2
        hq3 = length q3
        hq4 = length q4
        cH  = hq0 `div` 2
        height = sum [hq0, hq1, hq2, hq3, hq4]

stack8Layout :: Int -> [String]
stack8Layout size =  hcat [ fanout height
                          , vConnect height $ scanl (+) cH [hq0, hq1, hq2, hq3, hq4, hq5, hq6]
                          , concat [q0, q1, q2, q3, q4, q5, q6, q7]
                          , vConnect height $ scanl (+) 1 [hq0, hq1, hq2, hq3, hq4, hq5, hq6] ++ scanl (+) hq0 [hq1, hq2, hq3, hq4, hq5, hq6, hq7]
                          , fanout height
                          ]
  where q0 = makeLayout [0,16..size-1]
        q1 = makeLayout [2,18..size-1]
        q2 = makeLayout [4,20..size-1]
        q3 = makeLayout [6,22..size-1]
        q4 = makeLayout [8,24..size-1]
        q5 = makeLayout [10,26..size-1]
        q6 = makeLayout [12,28..size-1]
        q7 = makeLayout [14,30..size-1]
        hs = map length [q0,q1,q2,q3,q4,q5,q6,q7]
        [hq0,hq1,hq2,hq3,hq4,hq5,hq6,hq7] = hs
        cH  = hq0 `div` 2
        height = sum hs


-- head, column, tailer で 3 列以上つまり 32 * 3 = 96 以上のサイズが必要になる。
-- stack16 は 16 rows の制御バスを持つ。制御バスの上下にメモリマットを配置するので縦に32セル持つ。
stack16Layout :: Int -> [String]
stack16Layout size =  hcat [ fanout height
                            , vConnect height $ scanl (+) cH hs
                            , concat [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf]
                            , vConnect height $ scanl (+) 1 (init hs) ++ scanl (+) hq0 (tail hs)
                            , fanout height
                            ]
  where qs = [ makeLayout [i,i+32..size-1] | i <- [0,2..31] ]
        [q0,q1,q2,q3,q4,q5,q6,q7,q8,q9,qa,qb,qc,qd,qe,qf] = qs
        hs = map length qs
        [hq0,hq1,hq2,hq3,hq4,hq5,hq6,hq7,hq8,hq9,hqa,hqb,hqc,hqd,hqe,hqf] = hs
        cH  = hq0 `div` 2
        height = sum hs

-- 10 * 10 = 100 メモリマット
mem100 :: [String]
mem100 = stack5Layout 100
-- 8 * 16 = 128 メモリマット
mem128 :: [String]
mem128 = stack4Layout 128
-- 8 * 32 = 256 メモリマット
mem256 :: [String]
mem256 = stack4Layout 256
-- 32 * 16 = 512 メモリマット
mem512 :: [String]
mem512 = stack16Layout 512
-- 32 * 32 = 1024 メモリマット
mem1024 :: [String]
mem1024 = stack16Layout 1024




halfHeader :: Int -> [String]
halfHeader n = [ "+--------"
               , "|>@Rv    "
               , "|^ S<    "
               , "+--------"
               , "v        "
               , "v        "
               ]
               ++ judge n ++
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
               
halfColumn :: Int -> [String]
halfColumn n = [ "---------"
               , "         "
               , "         "
               , "---------"
               , "v        "
               , "v        "
               ]
               ++ judge n ++
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

halfTailer :: Int -> [String]
halfTailer n = [ "--------+"
               , "        |"
               , "        |"
               , "--------+"
               , "v        "
               , "v        "
               ]
               ++ judge n ++
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

halfBody :: [Int] -> [String]
halfBody = hcat . map halfColumn

makeHalfLayout :: [Int] -> [String]
makeHalfLayout xs = hcat [halfHeader s, halfBody ns, halfTailer e]
  where s = head xs
        e = last xs
        ns = tail (init xs)

-- makeHalfLayout を使って rows * cols のメモリマットを作る
singleLayout :: Int -> Int -> [String]
singleLayout rows cols = hcat [ fanout height
                              , vConnect height $
                                scanl (+) 1 [length b | b <- init body]
                              , concat body
                              , vConnect height $
                                scanl (+) (length (head body)) [length b | b <- tail body]
                              , fanout height
                              ]
  where nss = [[rows * c + r| c <- [0..cols-1]] | r <- [0..rows-1]]
        body = [makeHalfLayout ns | ns <- nss]
        height = sum [length b | b <- body]

    
history :: String
history = "1996: Philadelphia, PA, USA \"Optimality and inefficiency: What isn't a cost model of the lambda calculus?\" (Julia Lawall and Harry Mairson); 1997: Amsterdam, Netherlands \"Functional reactive animation\" (Conal Elliott and Paul Hudak); 1998: Baltimore, MD, USA \"Cayenne - a language with dependent types\" (Lennart Augustsson); 1999: Paris, France \"Haskell and XML: Generic combinators or type-based translation?\" (Malcolm Wallace and Colin Runciman); 2000: Montreal, Canada \"QuickCheck: a lightweight tool for random testing of Haskell programs\" (Koen Claessen and John Hughes); 2001: Florence, Italy \"Recursive Structures for Standard ML\" (Claudio Russo); 2002: Pittsburgh, PA, USA \"Contracts for higher-order functions\" (Robert Findler and Matthias Felleisen); 2003: Uppsala, Sweden \"MLF: Raising ML to the Power of System F\" (Didier Le Botlan and Didier Remy); 2004: Snowbird, UT, USA \"Scrap More Boilerplate: Reflection, Zips, and Generalised Casts\" (Ralf Lammel and Simon Peyton Jones); 2005: Tallinn, Estonia \"Associated Type Synonyms\" (Manuel M. T. Chakravarty, Gabriele Keller, and Simon Peyton Jones); 2006: Portland, OR, USA \"Simple unification-based type inference for GADTs\" (Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, and Geoffrey Washburn); 2007: Freiburg, Germany \"Ott: Effective Tool Support for the Working Semanticist\" (Peter Sewell, Francesco Zappa Nardelli, Scott Owens, Gilles Peskine, Thomas Ridge, Susmit Sarkar, and Rok Strnisa); 2008: Victoria, BC, Canada \"Parametric higher-order abstract syntax for mechanized semantics\" (Adam Chlipala); 2009: Edinburgh, UK \"Runtime Support for Multicore Haskell\" (Simon Marlow, Simon Peyton Jones, and Satnam Singh); 2010: Baltimore, MD, USA \"Abstracting abstract machines\" (David Van Horn and Matthew Might); 2011: Tokyo, Japan \"Frenetic: a network programming language\" (Nate Foster, Rob Harrison, Michael Freedman, Christopher Monsanto, Jennifer Rexford, Alex Story, and David Walker); 2012: Copenhagen, Denmark \"Addressing covert termination and timing channels in concurrent information flow systems\" (Deian Stefan, Alejandro Russo, Pablo Buiras, Amit Levy, John C. Mitchell and David Mazieres); 2013: Boston, MA, USA \"Handlers in Action\" (Ohad Kammar, Sam Lindley, and Nicolas Oury); 2014: Gothenburg, Sweden \"Refinement Types for Haskell\" (Niki Vazou, Eric L. Seidel, Ranjit Jhala, Dimitrios Vytiniotis, and Simon Peyton-Jones); 2015: Vancouver, BC, Canada \"1ML - core and modules united (F-ing first-class modules)\" (Andreas Rossberg); 2016: Nara, Japan; 2017: Oxford, UK; 2018: St. Louis, MO, USA; 2019: Berlin, Germany; 2020: Jersey City, NJ, USA (virtual); 2021: Daejeon, South Korea (virtual); 2022: Ljubljana, Slovenia; 2023: Seattle, WA, USA; 2024: Milan, Italy; 2025: Singapore, Singapore; 2026: Indianapolis, IN, USA"

enc :: Char -> String
enc c = let fc = f c
            gc = g c
        in if length fc < length gc then fc else gc
  where
    f c = let n = ord c in if n < 10 then show n else "`" ++ show n ++ "`"
    g c = let n = ord c `xor` 96 in if n < 10 then show n ++ "~" else "`" ++ show n ++ "`~"

enc' :: String -> String
enc' text =
  let x = "@`96`W" ++ concatMap (\c -> enc c ++ "S") text ++ "H"
      n = length x
  in "+" ++ replicate n '-' ++ "+\n" ++
     "|" ++ x ++ "|\n" ++
     "+" ++ replicate n '-' ++ "+\n"

enc'' :: String -> String
enc'' text = let body = "`96`W" ++ concatMap (\c -> enc c ++ "S") text ++ "H"
             in "+--+\n" ++ "|@v|\n" ++ unlines (map (\c -> "| " ++ [c] ++ "|") body) ++ "+--+\n"
