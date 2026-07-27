{-# OPTIONS -Wno-x-partial #-}

module MemoryLayout where


--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

-- head, column, tailer で 3 列以上つまり 6 * 3 = 18 以上のサイズが必要になる。
-- stack3 は 3 rows の制御バスを持つ。制御バスの上下にメモリマットを配置するので縦に6セル持つ。
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

--------------------------------------------------------------------------------

addIOtoTop :: [String] -> [String]
addIOtoTop     []   = error "addIOtop: null input"
addIOtoTop xs@(x:_) = io ++ xs
  where
    io = zipWith
      (\i o -> i ++ pad ++ o)
      input output
    pad = replicate plen ' '
    plen = width - length (head input) - length (head output)
    width = length x
    input =
      [ "  +-+"
      , "  |I|"
      , "v<+-+"
      ]
    output =
      [ "+-+ "
      , "|O|<"
      , "+-+^"
      ]

--------------------------------------------------------------------------------

mem2_100 :: [String]
mem2_100 = stack2Layout 100

mem3_102 :: [String]
mem3_102 = stack3Layout 102
