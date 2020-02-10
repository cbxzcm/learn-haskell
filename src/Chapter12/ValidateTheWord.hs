module Chapter12.ValidateTheWord where

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord []    = Nothing
mkWord input = case foldr countVowels (0, 0) input of
    (a, b) -> if a > b then Nothing else Just (Word' input)

countVowels :: Char -> (Integer, Integer) -> (Integer, Integer)
countVowels c (numVowels, numConsonants) = if c `elem` vowels
    then (numVowels + 1, numConsonants)
    else (numVowels, numConsonants + 1)
