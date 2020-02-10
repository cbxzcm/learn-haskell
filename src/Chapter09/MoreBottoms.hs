module Chapter09.MoreBottoms where
import Data.Bool

-- undefined
a = take 1 $ map (+1) [undefined, 2, 3]

-- [2]
b = take 1 $ map (+1) [1, undefined, 3]

-- undefined
c = take 2 $ map (+1) [1, undefined, 3]

-- For each characgter in string xs, return a bool value indicated if the character is a vowel (lowercase)
itIsMystery xs = 
  map (\x -> elem x "aeiou") xs

-- Numbers 1 to 10 squared
d = map (^2) [1..10]

-- Smallest value of each inner list: [1, 10, 20]
e = map minimum [[1..10], [10..20], [20..30]]

-- [15, 15, 15]
f = map sum [[1..5], [1..5], [1..5]]


myFoldBool :: a -> a -> Bool -> [a]
myFoldBool x y b = map (\input -> if b then y else x) [x]
