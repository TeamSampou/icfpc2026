module Exam () where

data Instr
  = R
  | S
  | Lit Integer
  | M
  | W
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | Halt
  | Man
  deriving Eq

instance Show Instr where
  show R       = "R"
  show S       = "S"
  show (Lit n) = lit n
  show M       = "M"
  show W       = "W"
  show Add     = "+"
  show Sub     = "-"
  show Mul     = "*"
  show Div     = "/"
  show Neg     = "-"
  show Man     = "@"
  show Halt    = "H"

pack :: [Instr] -> String
pack instrs = concatMap show instrs'
  where instrs' = [Man] ++ instrs ++ [Halt]

lit :: Integer -> String
lit n | n < 10 = show n
      | otherwise = "`" ++ show n ++ "`"

-- r + r
addrr :: [Instr]
addrr = [R, M, R, W, Add, S]
-- n + r
addnr :: Integer -> [Instr]
addnr n = [Lit n, M, R, W, Add, S]
-- r + n
addrn :: Integer -> [Instr]
addrn n = [R, M, Lit n, W, Add, S]

-- r - r
subrr :: [Instr]
subrr = [R, M, R, W, Sub, S]
-- n - r
subnr :: Integer -> [Instr]
subnr n = [Lit n, M, R, W, Sub, S]
-- r - n
subrn :: Integer -> [Instr]
subrn n = [R, M, Lit n, W, Sub, S]

-- r * r
mulrr :: [Instr]
mulrr = [R, M, R, W, Mul, S]
-- n * r
mulnr :: Integer -> [Instr]
mulnr n = [Lit n, M, R, W, Mul, S]
-- r * n
mulrn :: Integer -> [Instr]
mulrn n = [R, M, Lit n, W, Mul, S]

-- r / r
divrr :: [Instr]
divrr = [R, M, R, W, Div, S]
-- n / r
divnr :: Integer -> [Instr]
divnr n = [Lit n, M, R, W, Div, S]
-- r / n
divrn :: Integer -> [Instr]
divrn n = [R, M, Lit n, W, Div, S]

fanOut :: [Instr]
fanOut = [R, S]
