module Chapter15.MaybeAnotherMonoid where

import Chapter15.OptionalMonoid (Optional (..))
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    frequency,
    quickCheck,
  )

newtype First' a = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) x@(First' (Only _)) (First' Nada) = x
  (<>) (First' Nada) y@(First' (Only _)) = y
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    let nada = pure $ First' Nada
    let only = First' . Only <$> arbitrary

    frequency [(1, nada), (1, only)]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
