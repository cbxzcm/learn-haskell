module Chapter09.SquareCube where

mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube = [y ^ 3 | y <- [1 .. 5]]

-- 1
myTuples = [(x, y) | x <- mySqr, y <- myCube]

-- 2
myTuplesLessThan50 = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

-- 3
myTuplesLessThan50Count = length myTuplesLessThan50
