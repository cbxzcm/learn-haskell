module Chapter07.CasePractice where

functionC x y = if x > y then x else y

ifEvenAdd2 n = if even n then n + 2 else n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
