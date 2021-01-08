module Chapter14.UsingQuickCheck where

import Data.List (sort)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    Testable (property),
    forAll,
    suchThat,
  )

-- Generators
positiveIntGen :: Gen Int
positiveIntGen = (arbitrary :: Gen Int) `suchThat` (> 0)

positiveIntTupleGen :: Gen (Int, Int)
positiveIntTupleGen = do
  a <- positiveIntGen
  b <- positiveIntGen
  return (a, b)

-- 1
half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_listOrdered_sorted :: [Int] -> Bool
prop_listOrdered_sorted = listOrdered . sort

-- 3
plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

-- 4
timesAssociative :: Int -> Int -> Int -> Bool
timesAssociative x y z = x * (y * z) == (x * y) * z

timesCommutative :: Int -> Int -> Bool
timesCommutative x y = x * y == y * x

-- 5
quoteRemRelationship :: Int -> Int -> Bool
quoteRemRelationship x y = quot x y * y + rem x y == x

divModRelationship :: Int -> Int -> Bool
divModRelationship x y = div x y * y + mod x y == x

prop_quoteRemRelationship :: Property
prop_quoteRemRelationship = forAll positiveIntTupleGen $ uncurry quoteRemRelationship

prop_divModRelationship :: Property
prop_divModRelationship = forAll positiveIntTupleGen $ uncurry divModRelationship

-- 6
-- Exponentiation isn't associative. This property should fail
powAssociative :: Int -> Int -> Int -> Bool
powAssociative x y z = x ^ (y ^ z) == (z ^ y) ^ z

-- Exponentiation isn't commutative. This property should fail
powCommutative :: Int -> Int -> Bool
powCommutative x y = x ^ y == y ^ x

-- 7
doubleReverseEqualsId :: [Int] -> Bool
doubleReverseEqualsId list = (reverse . reverse) list == id list

-- 8
prop8a :: (Bool -> Bool) -> Bool -> Bool
prop8a f a = f $ a == f a

prop8b :: (Bool -> Bool) -> (Bool -> Bool) -> Bool -> Bool
prop8b f g x = (f . g) x == (\x -> f (g x)) x

-- 9
-- This isn't a valid property. Example: foldr (++) [] [0] [1] != (++) [0] [1]
prop9a :: [Int] -> [Int] -> Bool
prop9a x y = foldr (:) x y == (++) x y

prop9b :: [[Int]] -> Bool
prop9b x = foldr (++) [] x == concat x

-- 10
-- This isn't a valid property. Example: length (take 1 []) != 1
prop10 :: Int -> [Int] -> Bool
prop10 n xs = length (take n xs) == n

-- 11
prop11 :: Int -> Bool
prop11 x = read (show x) == x

-- Run property tests
main :: IO ()
main = hspec $ do
  describe "1" $ do
    it "half identity is always equal to x" $ do
      property prop_halfIdentity
  describe "2" $ do
    it "listOrdered is always True on a sorted list" $ do
      property prop_listOrdered_sorted
  describe "3" $ do
    it "addition of integers is associative" $ do
      property plusAssociative
    it "addition of integers is commutative" $ do
      property plusCommutative
  describe "4" $ do
    it "multiplication of integers is associative" $ do
      property timesAssociative
    it "multiplication of integers is commutative" $ do
      property timesCommutative
  describe "5" $ do
    it "quot is related to rem" $ do
      prop_quoteRemRelationship
    it "div is related to mod" $ do
      prop_quoteRemRelationship
  describe "6" $ do
    it "exponent of integers is associative" $ do
      property powAssociative
    it "exponent of integers is commutative" $ do
      property powCommutative
  describe "7" $ do
    it "Reversing a list twice is equal to identity" $ do
      property doubleReverseEqualsId
  describe "8" $ do
    it "f $ a = f a" $ do
      property (prop8a id)
    it "f . g = \\x -> f (g x)" $ do
      property (prop8b id id)
  describe "9" $ do
    it "foldr (:) == (++)" $ do
      property prop9a
    it "foldr (++) [] == concat" $ do
      property prop9b
  describe "10" $ do
    it "f n xs = length (take n xs) == n" $ do
      property prop10
  describe "11" $ do
    it "f x = (read (show x)) == x" $ do
      property prop11