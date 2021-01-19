{-# LANGUAGE TupleSections #-}

module Chapter15.MonoidExercises where

import Data.Monoid (Sum)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    applyArbitrary2,
    quickCheck,
  )

-- Monoid laws
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = a <> mempty == a

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivAssoc =
  Trivial ->
  Trivial ->
  Trivial ->
  Bool

type TrivIdentity = Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity $ x <> y

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

type IdentityAssoc =
  Identity String ->
  Identity String ->
  Identity String ->
  Bool

type IdentityIdentity = Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = applyArbitrary2 Two

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x1 y1) (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

type TwoAssoc =
  Two String String ->
  Two String String ->
  Two String String ->
  Bool

type TwoIdentity = Two String String -> Bool

-- 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Semigroup BoolConj where
  (<>) (BoolConj x) (BoolConj y) = BoolConj $ x && y

instance Monoid BoolConj where
  mempty = BoolConj True

type BoolConjAssoc =
  BoolConj ->
  BoolConj ->
  BoolConj ->
  Bool

type BoolConjIdentity = BoolConj -> Bool

-- 5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

instance Semigroup BoolDisj where
  (<>) (BoolDisj x) (BoolDisj y) = BoolDisj $ x || y

instance Monoid BoolDisj where
  mempty = BoolDisj False

type BoolDisjAssoc =
  BoolDisj ->
  BoolDisj ->
  BoolDisj ->
  Bool

type BoolDisjIdentity = BoolDisj -> Bool

-- 6
newtype Combine a b = Combine {unCombine :: a -> b}

instance (Eq a, Show a, Eq b, Show b, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine . const <$> arbitrary

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \a -> f a <> g a

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ const mempty

instance Show (Combine a b) where
  show _ = "An arbitrary Combine function"

type CombineAssoc =
  Combine Int (Sum Int) ->
  Combine Int (Sum Int) ->
  Combine Int (Sum Int) ->
  Int ->
  Bool

type CombineIdentity = Combine Int (Sum Int) -> Int -> Bool

combineSemigroupAssoc :: (Eq m, Semigroup m) => Combine a m -> Combine a m -> Combine a m -> a -> Bool
combineSemigroupAssoc a b c x = unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c) x

combineMonoidLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineMonoidLeftIdentity a x = unCombine (mempty <> a) x == unCombine a x

combineMonoidRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineMonoidRightIdentity a x = unCombine (a <> mempty) x == unCombine a x

-- 7
newtype Comp a = Comp {unComp :: a -> a}

instance Show (Comp a) where
  show _ = "a -> a"

-- My rationale:  a -> a is the id function. There is only one id function.
instance Arbitrary (Comp a) where
  arbitrary = pure $ Comp id

-- My rationale: a -> a is the id function. There is only one id function. Therefore id is equal to itself.
instance Eq (Comp a) where
  (==) _ _ = True

-- My rationale: a -> a is the id function. Therefore, id . id == id
instance Semigroup a => Semigroup (Comp a) where
  (<>) _ _ = Comp id

instance Monoid a => Monoid (Comp a) where
  mempty = Comp id

type CompAssoc =
  Comp String ->
  Comp String ->
  Comp String ->
  Bool

type CompIdentity = Comp String -> Bool

-- 8
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem combined
    where
      first x = fst (f x) <> fst (g x)
      second = snd . g . snd . f
      combined x = (first x, second x)

-- Requires TupleSections language extension
instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (mempty,)

-- Run tests
main :: IO ()
main = hspec $ do
  describe "1. Trivial Monoid" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: TrivAssoc)
    it "satisfies left identity law" $ do
      quickCheck (monoidLeftIdentity :: TrivIdentity)
    it "satisfies right identity law" $ do
      quickCheck (monoidRightIdentity :: TrivIdentity)
  describe "2. Indentity Monoid" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: IdentityAssoc)
    it "satisfies left identity law" $ do
      quickCheck (monoidLeftIdentity :: IdentityIdentity)
    it "satisfies right identity law" $ do
      quickCheck (monoidRightIdentity :: IdentityIdentity)
  describe "3. Two Monoid" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: TwoAssoc)
    it "satisfies left identity law" $ do
      quickCheck (monoidLeftIdentity :: TwoIdentity)
    it "satisfies right identity law" $ do
      quickCheck (monoidRightIdentity :: TwoIdentity)
  describe "4. BoolConj Monoid" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: BoolConjAssoc)
    it "satisfies left identity law" $ do
      quickCheck (monoidLeftIdentity :: BoolConjIdentity)
    it "satisifies right identity law" $ do
      quickCheck (monoidRightIdentity :: BoolConjIdentity)
  describe "5. BoolDisj Monoid" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: BoolDisjAssoc)
    it "satisfies left identity law" $ do
      quickCheck (monoidLeftIdentity :: BoolDisjIdentity)
    it "satisifies right identity law" $ do
      quickCheck (monoidRightIdentity :: BoolDisjIdentity)
  describe "6. Combine Monoid" $ do
    it "is associative" $ do
      quickCheck (combineSemigroupAssoc :: CombineAssoc)
    it "satisfies left identity law" $ do
      quickCheck (combineMonoidLeftIdentity :: CombineIdentity)
    it "satisfies rigjht identity law" $ do
      quickCheck (combineMonoidRightIdentity :: CombineIdentity)
  describe "7. Comp Monoid" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: CompAssoc)
    it "satisfies left identity law" $ do
      quickCheck (monoidLeftIdentity :: CompIdentity)
    it "satisfies right identity law" $ do
      quickCheck (monoidRightIdentity :: CompIdentity)
  describe "8. Mem Monoid" $ do
    let f' :: Mem Int String
        f' = Mem $ \s -> ("hi", s + 1)
        rmzero :: (String, Int)
        rmzero = runMem mempty 0
        rmleft :: (String, Int)
        rmleft = runMem (f' <> mempty) 0
        rmright :: (String, Int)
        rmright = runMem (mempty <> f') 0

    it "rmleft" $ do
      rmleft `shouldBe` ("hi", 1)
    it "rmright" $ do
      rmright `shouldBe` ("hi", 1)
    it "rmzero :: (String, Int)" $ do
      (rmzero :: (String, Int)) `shouldBe` ("", 0)
    it "rmleft == runMem f' 0" $ do
      rmleft == runMem f' 0 `shouldBe` True
    it "rmright == runMem f' 0" $ do
      rmright == runMem f' 0 `shouldBe` True
