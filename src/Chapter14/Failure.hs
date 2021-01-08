module Chapter14.Failure where

import Data.List (sort)
import Test.Hspec ()
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
  )

positiveDoubleGen :: Gen Double
positiveDoubleGen = fmap abs (arbitrary :: Gen Double) `suchThat` (> 0)

square :: Num a => a -> a
square x = x * x

-- Find out why this property fails
squareIdentity :: Double -> Double
squareIdentity = square . sqrt

prop_squareIdentity :: Property
prop_squareIdentity = forAll positiveDoubleGen (\x -> square x == squareIdentity x)

main :: IO ()
main = quickCheck prop_squareIdentity