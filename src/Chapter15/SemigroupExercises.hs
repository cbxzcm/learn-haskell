module Chapter15.SemigroupExercises where

import Data.Semigroup (Sum)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Testable (property),
    applyArbitrary2,
    applyArbitrary3,
    applyArbitrary4,
    frequency,
    quickCheck,
  )

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity $ x <> y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x1 y1) (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = applyArbitrary2 Two

type TwoAssoc =
  Two String String ->
  Two String String ->
  Two String String ->
  Bool

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three x1 y1 z1) (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = applyArbitrary3 Three

type ThreeAssoc =
  Three String String String ->
  Three String String String ->
  Three String String String ->
  Bool

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four w1 x1 y1 z1) (Four w2 x2 y2 z2) = Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = applyArbitrary4 Four

type FourAssoc =
  Four String String String String ->
  Four String String String String ->
  Four String String String String ->
  Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj x) (BoolConj y) = BoolConj $ x && y

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc =
  BoolConj ->
  BoolConj ->
  BoolConj ->
  Bool

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj x) (BoolDisj y) = BoolDisj $ x || y

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc =
  BoolDisj ->
  BoolDisj ->
  BoolDisj ->
  Bool

-- 8
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Fst x) (Fst y) = Fst y
  (<>) (Fst x) (Snd y) = Snd y
  (<>) (Snd x) (Fst y) = Snd x
  (<>) (Snd x) (Snd y) = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

type OrAssoc =
  Or String String ->
  Or String String ->
  Or String String ->
  Bool

type OrExample = Or Int Int

-- 9
newtype Combine a b = Combine {unCombine :: a -> b}

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \a -> f a <> g a

instance (Eq a, Show a, Eq b, Show b, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine . const <$> arbitrary

instance Show (Combine a b) where
  show _ = "An arbitrary Combine function"

type CombineAssoc =
  Combine Int (Sum Int) ->
  Combine Int (Sum Int) ->
  Combine Int (Sum Int) ->
  Int ->
  Bool

combineIntSum :: Combine Int (Sum Int) -> Combine Int (Sum Int) -> Int -> Bool
combineIntSum f@(Combine f') g@(Combine g') x = unCombine (f <> g) x == f' x <> g' x

combineSemigroupAssoc :: (Eq m, Semigroup m) => Combine a m -> Combine a m -> Combine a m -> a -> Bool
combineSemigroupAssoc a b c x = unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c) x

-- 10
newtype Comp a = Comp {unComp :: a -> a}

-- My rationale: a -> a is the id function. Therefore, id . id == id
instance Semigroup a => Semigroup (Comp a) where
  (<>) _ _ = Comp id

-- My rationale:  a -> a is the id function. There is only one id function.
instance Arbitrary (Comp a) where
  arbitrary = pure $ Comp id

-- My rationale: a -> a is the id function. There is only one id function. Therefore id is equal to itself.
instance Eq (Comp a) where
  (==) _ _ = True

instance Show (Comp a) where
  show _ = "a -> a"

type CompAssoc =
  Comp String ->
  Comp String ->
  Comp String ->
  Bool

-- 11
data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure x) (Failure y) = Failure $ x <> y
  (<>) (Success x) _ = Success x
  (<>) _ (Success y) = Success y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    let successValue = Success <$> arbitrary
    let failureValue = Failure <$> arbitrary

    frequency [(1, successValue), (1, failureValue)]

type ValidationAssoc =
  Validation String String ->
  Validation String String ->
  Validation String String ->
  Bool

main :: IO ()
main = hspec $ do
  describe "1. Trivial Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: TrivAssoc)
  describe "2. Identity Semigroup" $ do
    it " is associative" $ do
      quickCheck (semigroupAssoc :: IdentityAssoc)
  describe "3. Two Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: TwoAssoc)
  describe "4. Three Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: ThreeAssoc)
  describe "5. Four Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: FourAssoc)
  describe "6. BoolConj Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: BoolConjAssoc)
    it "evaluates to BoolConj True for BoolConj True <> BoolConj True" $ do
      BoolConj True <> BoolConj True `shouldBe` BoolConj True
    it "evaluates to BoolConj False for BoolConj True <> BoolConj False" $ do
      BoolConj True <> BoolConj False `shouldBe` BoolConj False
  describe "7. BoolDisj Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: BoolDisjAssoc)
    it "evaluates to BoolDisj True for BoolDisj True <> BoolDisj True" $ do
      BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
    it "evaluates to BoolDisj True for BoolDisj True <> BoolDisj False" $ do
      BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
  describe "8. Or Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: OrAssoc)
    it "evaluates to Snd 2 for Fst 1 <> Snd 2" $ do
      Fst 1 <> Snd 2 `shouldBe` (Snd 2 :: OrExample)
    it "evaluates to Fst 2 for Fst 1 <> Fst 2" $ do
      Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: OrExample)
    it "evaluates to Snd 1 for Snd 1 <> Fst 2" $ do
      Snd 1 <> Fst 2 `shouldBe` (Snd 1 :: OrExample)
    it "evaluates to Snd 1 for Snd 1 <> Snd 2" $ do
      Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: OrExample)
  describe "9. Combine Semigroup" $ do
    it "is associative" $ do
      quickCheck (combineSemigroupAssoc :: CombineAssoc)
    it "Various Combine Int (Sum Int) examples" $ do
      quickCheck combineIntSum
  describe "10. Comp Semigroup" $ do
    it "is associative" $ do
      quickCheck (semigroupAssoc :: CompAssoc)
  describe "11. Validation Semigroup" $ do
    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success

    it "is associative" $ do
      quickCheck (semigroupAssoc :: ValidationAssoc)
    it "success 1 <> failure \"blah\"" $ do
      success 1 <> failure "blah" `shouldBe` Success 1
    it "failure \"woot\" <> failure \"blah\"" $ do
      failure "woot" <> failure "blah" `shouldBe` Failure "wootblah"
    it "success 1 <> success 2" $ do
      success 1 <> success 2 `shouldBe` Success 1
    it "failure \"woot\" <> success 2" $ do
      failure "woot" <> success 2 `shouldBe` Success 2
