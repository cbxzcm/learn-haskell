module Chapter09.BottomMadness where

-- undefined
a = [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

-- [1]
b = take 1 $ [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

-- undefined
c = sum [1, undefined, 3]

-- 3
d = length [1, 2, undefined]

-- undefined. caused by (++)
e = length $ [1, 2, 3] ++ undefined

-- [2]
f = take 1 $ filter even [1, 2, 3, undefined]

-- undefined
g = take 1 $ filter even [1, 3, undefined]

-- [1]
h = take 1 $ filter odd [1, 3, undefined]

-- [1, 3]
i = take 2 $ filter odd [1, 3, undefined]
