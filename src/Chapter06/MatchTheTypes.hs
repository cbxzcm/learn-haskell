module Chapter06.MatchTheTypes where

import Data.List

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: a -> Int
sigmund x = myX

sigmund' :: Num a => a -> Int
sigmund' x = myX

jung :: [Int] -> Int
jung = minimum

young :: Ord a => [a] -> a
young = minimum

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
