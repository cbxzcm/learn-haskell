module Chapter10.ScanExercises where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

fibs = map fibonacci [1 ..]

-- 1. Modify your fibs function to only return the first 20 Fibonacci numbers.
fibsN n = take n fibs

-- 2. Modify fibs ot return the Fibonacci numbers that ar eless than 100.
fibsNLT n lt = take n $ takeWhile (< lt) fibs

-- 3. Try to write a factorial function
factorial :: Int -> [Int]
factorial n = take n $ scanl (*) 1 [2 ..]
