module Chapter09.Exercises where

import Data.Char

-- 1
-- *Chapter9Exercises> :t isUpper
-- isUpper :: Char -> Bool
-- *Chapter9Exercises> :t toUpper
-- toUpper :: Char -> Char

-- 2
filterIsUpper :: String -> String
filterIsUpper = filter isUpper

-- 3
firstCharToUpper :: String -> String
firstCharToUpper [] = [] 
firstCharToUpper (c:cs) = toUpper c : cs

-- 4
allCharToUpper :: String -> String
allCharToUpper [] = []
allCharToUpper (c:cs) = toUpper c : allCharToUpper cs

-- 5, 6
-- *Chapter9Exercises> :t head
-- head :: [a] -> a
headToUpper :: String -> Char
headToUpper = toUpper . head 
