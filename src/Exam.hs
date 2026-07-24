module Exam () where

lit :: Integer -> String
lit n | n < 10 = show n
      | otherwise = "`" ++ show n ++ "`"

-- r + r
add2 :: String
add2 = "@RMR+SH"

-- r + n or n + r
add1 :: Integer -> String
add1 n = "@" ++ lit n ++ "MR+SH"

-- r - r
sub2 :: String
sub2 = "@RMR-SH"

-- r - n
sub1l :: Integer -> String
sub1l n = "@" ++ lit n ++ "MR-SH"
-- n - r
sub1r :: Integer -> String
sub1r n = "@" ++ lit n ++ "MRW-SH"


-- r * r
mul2 :: String
mul2 = "@RMR*SH"

-- x * n or n * x
mul1 :: Integer -> String
mul1 n = "@" ++ lit n ++ "MR*SH"

-- r / r
div2 :: String
div2 = "@RMR/SH"
-- r / n
div1l :: Integer -> String
div1l n = "@" ++ lit n ++ "MR/SH"
-- n / r
div1r :: Integer -> String
div1r n = "@" ++ lit n ++ "MRW/SH"


fanOut :: String
fanOut = "@RSH"
