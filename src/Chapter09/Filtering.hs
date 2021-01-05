module Chapter09.Filtering where

-- 1
multiplesOfThree :: Integral a => [a] -> [a]
multiplesOfThree = filter (\x -> rem x 3 == 0)

-- 2
multiplesOfThreeComp :: Integral a => [a] -> Int
multiplesOfThreeComp = length . filter (\x -> rem x 3 == 0)

-- 3
myFilter :: String -> [String]
myFilter = filter (/= "the") . filter (/= "a") . filter (/= "an") . words
