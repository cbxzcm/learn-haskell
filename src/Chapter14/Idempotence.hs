module Chapter14.Idempotence where

import Chapter11.LanguageExercises (capitalizeWord)
import Data.List (sort)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Testable (property))

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

f :: String -> Bool
f x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

f' :: [Int] -> Bool
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

main :: IO ()
main = hspec $ do
  describe "capitalizeWord" $ do
    it "is idempotent" $ do
      property f
  describe "sort" $ do
    it "is idempotent" $ do
      property f'