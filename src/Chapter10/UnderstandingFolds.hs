module Chapter10.UnderstandingFolds where

import Data.List

-- 1 TODO Verify
a1 = foldr (*) 1 [1 .. 5]

b1 = foldl (flip (*)) 1 [1 .. 5]

c1 = foldl (*) 1 [1 .. 5]

-- 2 Write out the evaluation steps for: foldl (flip (*)) 1 [1..3]
--
-- (((1 * 1) * 2) * 3)
-- ((2 * (1 * 1)) * 3)
-- (3 * (2 * (1 * 1)))

-- 3 One difference between foldr and foldl is:
--
-- c) foldr, but not foldl, associates to the right

-- 4 Foldsa are catamorphisms, which means they are generally used to:
--
-- a) Reduce structure

-- 5
a2 = foldr (++) [] ["woot", "WOOT", "woot"]

b2 = foldr max [] ["fear", "is", "the", "little", "death"]

c2 = foldr (&&) True [True, False]

--- d) No, because of the True initial value
e2 = foldl (++) "" $ map show [1 .. 5]

f2 = foldl const 'a' [1 .. 5]

g2 = foldl const 0 "tacos"

h2 = foldr (flip const) 0 "curritos"

i2 = foldr (flip const) 'z' [1 .. 5]
