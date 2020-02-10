module Chapter13.Hangman where

import           Control.Monad                  ( forever )
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           Data.List                      ( intersperse )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           System.Random                  ( randomRIO )

newtype WordList = WordList [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
  where
    gameLength w =
        let l = length (w :: String) in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle {
    secretWord :: String,
    state :: [Maybe Char],
    guesses :: [Char],
    numWrongGuesses :: Int }

instance Show Puzzle where
    show (Puzzle _ discovered guessed numWrongGuesses) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
            ++ " Guessed so far: "
            ++ guessed
            ++ ", # incorrect so far: "
            ++ show numWrongGuesses

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word emptyGuesses [] 0
    where emptyGuesses = map (const Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) c = toLower c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c = toLower c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s numWrongGuesses) c = Puzzle
    word
    newFilledInSoFar
    (c : s)
    newNumWrongGuesses
  where
    zipper guessed wordChar guessChar =
        if toLower wordChar == toLower guessed then Just wordChar else guessChar

    newFilledInSoFar   = zipWith (zipper c) word filledInSoFar
    numWrong           = length . filter isNothing
    newNumWrongGuesses = if numWrong filledInSoFar <= numWrong newFilledInSoFar
        then numWrongGuesses + 1
        else numWrongGuesses

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn
                "This character was in the word, filling in the word accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed numWrongGuesses) =
    if numWrongGuesses >= 7
        then do
            putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
        else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) = if all isJust filledInSoFar
    then do
        putStrLn "You win!"
        exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is " ++ show puzzle
    putStr "Guess a letter: "

    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
