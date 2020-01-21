module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = []
eftBool False False = [False]
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, GT]
eftOrd EQ LT = [LT, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd GT EQ = [EQ, GT]
eftOrd GT LT = [LT, GT]

eftInt :: Int -> Int -> [Int]
eftInt x y  
  | x < y = [x, y]
  | x > y = [y, x]
  | x == y = [x]

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x < y = [x, y]
  | x > y = [y, x]
  | x == y = [x]

