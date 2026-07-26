module Exam where

import Data.Char (ord)
import Data.Map (Map)
import Data.Set (Set)
import Data.Bits (xor)

import Prelude hiding (div, subtract)

-- | 一旦 R や S だけ考える。近い遠いは気にせず拾えた順に第1引数第2引数として R で処理する
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
          , "|^ < s " ++ show d0 ++ "|"
          , "|^   r-`|"
          , "|^ S<<^<|"
          , "|^<     |"
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
  
column :: Int -> [String]
column n = [ "---------"
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
           [ "v        "
           , "v        "
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
           , "v        "
           , "v        "
           ]
           ++ judge n ++
           [ "v        "
           , "v        "
           ]
           ++ keepValue ++
           [ "v        "
           , "v        "
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
                          , vConnect height [1, hEven + 1]
                          , evens ++ odds
                          , vConnect height [hEven, hEven + hOdd]
                          , fanout height
                          ]
  where evens = makeLayout [0,2..size-1]
        odds  = makeLayout [1,3..size-1]
        hEven = length evens
        hOdd  = length odds
        height = hEven + hOdd

stack4Layout :: Int -> [String]
stack4Layout size =  hcat [ fanout height
                          , vConnect height $ scanl (+) 1 [hq0, hq1, hq2]
                          , q0 ++ q1 ++ q2 ++ q3
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
        height = hq0 + hq1 + hq2 + hq3

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
