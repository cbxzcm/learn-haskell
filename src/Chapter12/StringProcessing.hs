module Chapter12.StringProcessing where

import           Data.Char                      ( toLower )
notThe :: String -> Maybe String
notThe input = if elem "the" $ words input then Nothing else Just input

replaceThe :: String -> String
replaceThe input = case notThe input of
    Nothing -> unwords $ foldr (replaceWords "the" "a") [""] (words input)
    Just _  -> input
  where
    replaceWords :: String -> String -> String -> [String] -> [String]
    replaceWords from to x acc = if x == from then to : acc else x : acc

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input = countTheBeforeVowel' 0 (words input)  where
    countTheBeforeVowel' :: Integer -> [String] -> Integer
    countTheBeforeVowel' count []  = count
    countTheBeforeVowel' count [_] = count
    countTheBeforeVowel' count (first : second : tail) =
        if first == "the" && isVowelInitialWord second
            then countTheBeforeVowel' (count + 1) tail
            else countTheBeforeVowel' count tail

    isVowelInitialWord :: String -> Bool
    isVowelInitialWord []       = False
    isVowelInitialWord (x : xs) = toLower x `elem` ['a', 'e', 'i', 'o', 'u']

countVowels :: String -> Integer
countVowels = foldr countVowels' 0

countVowels' :: Char -> Integer -> Integer
countVowels' c count =
    if toLower c `elem` ['a', 'e', 'i', 'o', 'u'] then count + 1 else count