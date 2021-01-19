module Chapter15.OptionalMonoid where

import Control.Monad (join)
import Data.Monoid (Product, Sum)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    elements,
    forAll,
    frequency,
    suchThat,
  )

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only $ x <> y
  (<>) Nada (Only y) = Only y
  (<>) (Only x) Nada = Only x
  (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (1, Only <$> arbitrary)]

validLeftAndRight :: (Monoid a, Eq a) => Optional a -> Optional a -> Bool
validLeftAndRight x@(Only x') y@(Only y') = x `mappend` y == Only (x' `mappend` y')
validLeftAndRight x@(Only x') Nada = x == Only x'
validLeftAndRight Nada y@(Only y') = y == Only y'
validLeftAndRight Nada Nada = False

isOnly :: Optional a -> Bool
isOnly (Only _) = True
isOnly Nada = False

main :: IO ()
main = hspec $ do
  describe "Optional Monoid mempty" $ do
    it "should equal Nada" $ do
      (mempty :: Optional (Sum Int)) `shouldBe` Nada
  describe "Optional Monoid mappend" $ do
    it "should append Only left and right Sum values" $ do
      forAll
        ((arbitrary :: Gen (Optional (Sum Int))) `suchThat` isOnly)
        validLeftAndRight
    it "should append Only left and right Product values" $ do
      forAll
        ((arbitrary :: Gen (Optional (Product Int))) `suchThat` isOnly)
        validLeftAndRight
    it "should not append Nada left and right values" $ do
      validLeftAndRight (Nada :: Optional (Sum Int)) (Nada :: Optional (Sum Int)) `shouldBe` False
