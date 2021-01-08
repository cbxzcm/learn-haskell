module Chapter14.HangmanTest where

import Chapter13.Hangman (Puzzle (Puzzle, guesses, numWrongGuesses, secretWord, state), charInWord, fillInCharacter, freshPuzzle, handleGuess)
import Data.Char (toLower)
import Data.List (elemIndices)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Arbitrary (arbitrary), Property, elements, forAll, suchThat)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

newtype TestPuzzle = TestPuzzle Puzzle

instance Show TestPuzzle where
  show (TestPuzzle a) =
    "Puzzle{secretWord=\""
      ++ secretWord a
      ++ "\", state="
      ++ show (state a)
      ++ ", guesses="
      ++ guesses a
      ++ ", numWrongGuesses="
      ++ show (numWrongGuesses a)
      ++ "}"

instance Eq TestPuzzle where
  (==) (TestPuzzle x) (TestPuzzle y) =
    secretWord x == secretWord y
      && state x == state y
      && guesses x == guesses y
      && numWrongGuesses x == numWrongGuesses y

puzzleGen :: Gen TestPuzzle
puzzleGen = do
  secretWord <- map toLower <$> arbitrary `suchThat` (not . null)

  return (TestPuzzle $ freshPuzzle secretWord)

puzzleGuessGen :: Gen (TestPuzzle, Char)
puzzleGuessGen = do
  puzzle <- puzzleGen
  guess <- arbitrary

  return (puzzle, guess)

charNotInWord :: TestPuzzle -> Char -> Bool
charNotInWord (TestPuzzle a) guess = not $ charInWord a (toLower guess)

puzzleGuessGenMismatch :: Gen (TestPuzzle, Char)
puzzleGuessGenMismatch = do
  puzzle <- puzzleGen
  guess <- arbitrary `suchThat` charNotInWord puzzle

  return (puzzle, guess)

puzzleGuessGenMatch :: Gen (TestPuzzle, Char)
puzzleGuessGenMatch = do
  puzzle@(TestPuzzle a) <- puzzleGen
  guess <- elements (secretWord a)

  return (puzzle, guess)

puzzleGuessGenExistingMatch :: Gen (TestPuzzle, Char)
puzzleGuessGenExistingMatch = do
  (TestPuzzle a, guess) <- puzzleGuessGenMatch

  return (TestPuzzle $ fillInCharacter a guess, guess)

fillInCharacterSecretWordDoesNotChanged :: (TestPuzzle, Char) -> Bool
fillInCharacterSecretWordDoesNotChanged (TestPuzzle a, guess) =
  let newPuzzle = fillInCharacter a guess
   in secretWord a == secretWord newPuzzle

fillInCharacterUpdatePuzzleGuesses :: (TestPuzzle, Char) -> Bool
fillInCharacterUpdatePuzzleGuesses (TestPuzzle a, guess) =
  let newPuzzle = fillInCharacter a guess
   in length (guesses a) + 1 == length (guesses newPuzzle)

fillInCharacterStateNotChanged :: (TestPuzzle, Char) -> Bool
fillInCharacterStateNotChanged (TestPuzzle a, guess) =
  let newPuzzle = fillInCharacter a guess
   in state a == state newPuzzle

multiIndex :: [a] -> [Int] -> [a]
multiIndex [] _ = []
multiIndex _ [] = []
multiIndex list (x : xs) = (list !! x) : multiIndex list xs

isCharMatched :: Maybe Char -> Bool
isCharMatched Nothing = False
isCharMatched (Just _) = True

fillInCharacterStateChanged :: (TestPuzzle, Char) -> Bool
fillInCharacterStateChanged (TestPuzzle a, guess) =
  let newPuzzle = fillInCharacter a guess
      matchingIndices = elemIndices guess (secretWord a)
      oldStateElems = multiIndex (state a) matchingIndices
      newStateElems = multiIndex (state newPuzzle) matchingIndices
      newMatched = all isCharMatched newStateElems
      oldMatched = all isCharMatched oldStateElems
   in newMatched && not oldMatched

handleGuessAllStateNotChanged :: (TestPuzzle, Char) -> Property
handleGuessAllStateNotChanged (TestPuzzle a, guess) = monadicIO $ do
  newPuzzle <- run $ handleGuess a guess

  assert (TestPuzzle a == TestPuzzle newPuzzle)

handleGuessStateChanged :: (TestPuzzle, Char) -> Property
handleGuessStateChanged (TestPuzzle a, guess) = monadicIO $ do
  newPuzzle <- run $ handleGuess a guess

  assert (state a /= state newPuzzle && guesses a /= guesses newPuzzle)

handleGuessExistingGuessesChanged :: (TestPuzzle, Char) -> Property
handleGuessExistingGuessesChanged (TestPuzzle a, guess) = monadicIO $ do
  newPuzzle <- run $ handleGuess a guess

  assert $ numWrongGuesses a + 1 == numWrongGuesses newPuzzle && guesses a /= guesses newPuzzle

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "doesn't change Puzzle secretWord" $ do
      forAll puzzleGuessGen fillInCharacterSecretWordDoesNotChanged
    it "updates Puzzle guesses" $ do
      forAll puzzleGuessGen fillInCharacterUpdatePuzzleGuesses
    it "doesn't change Puzzle state for a mismatched guess" $ do
      forAll puzzleGuessGenMismatch fillInCharacterStateNotChanged
    it "changes Puzzle state for a matched guess" $ do
      forAll puzzleGuessGenMatch fillInCharacterStateChanged
  describe "handleGuess" $ do
    it "does nothing when we handle an existing guess" $ do
      forAll puzzleGuessGenExistingMatch handleGuessAllStateNotChanged
    it "updates Puzzle state and guesses for a matching guess" $ do
      forAll puzzleGuessGenMatch handleGuessStateChanged
    it "updates Puzzle guesses for a unmathced guess" $ do
      forAll puzzleGuessGenMismatch handleGuessExistingGuessesChanged
