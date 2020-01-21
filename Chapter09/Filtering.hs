module Filtering where

-- 1
multiplesOfThree :: Integral a => [a] -> [a]
multiplesOfThree xs = filter (\x -> (rem x 3) == 0) xs

-- 2
multiplesOfThreeComp :: Integral a => [a] -> Int
multiplesOfThreeComp = length . filter (\x -> (rem x 3) == 0)

-- 3
myFilter :: String -> [String]
myFilter = filter (\x -> x /= "the") . filter (\x -> x /= "a") . filter (\x -> x /= "an") . words
