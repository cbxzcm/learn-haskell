module Chapter11.AsPatterns where
import           Data.Char
import           Data.List.Split
import           Data.List

-- 1. This shouls return True if (and only if) all the values in the first list
-- appear in the second list, though they need not be contiguous

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf subSequence@(x : xs) (y : ys) | x == y = isSubseqOf xs ys
                                         | x /= y = isSubseqOf subSequence ys

isSubseqOfTest :: String -> String -> Bool -> IO ()
isSubseqOfTest subSequence sequence expected =
    if isSubseqOf subSequence sequence == expected
        then putStrLn
            (  "[PASS] subSequence ="
            ++ subSequence
            ++ ", sequence = "
            ++ sequence
            ++ ", expected = "
            ++ (show expected)
            )
        else putStrLn
            (  "[FAIL] subSequence ="
            ++ subSequence
            ++ ", sequence = "
            ++ sequence
            ++ ", expected = "
            ++ (show expected)
            )

runIsSubseqOfTests :: IO ()
runIsSubseqOfTests = do
    isSubseqOfTest "blah" "blahwoot" True
    isSubseqOfTest "blah" "wootblah" True
    isSubseqOfTest "blah" "wboloath" True
    isSubseqOfTest "blah" "wootbla"  False
    isSubseqOfTest "blah" "halbwoot" False
    isSubseqOfTest "blah" "blawhoot" True

-- 2. Split a sentence into words, then tuple each word with the capitalized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords input = fmap capitalizeWord (words input)
    where capitalizeWord originalWord@(x : xs) = (originalWord, toUpper x : xs)

capitalizeWordsTest :: String -> [(String, String)] -> IO ()
capitalizeWordsTest input expected = if capitalizeWords input == expected
    then
        putStrLn
            (  "[PASS] input = \""
            ++ input
            ++ "\", expected = "
            ++ (show expected)
            )
    else
        putStrLn
            (  "[FAIL] input = \""
            ++ input
            ++ "\", expected = "
            ++ (show expected)
            )

runCapitalizeWordsTests :: IO ()
runCapitalizeWordsTests = do
    capitalizeWordsTest "hello world" [("hello", "Hello"), ("world", "World")]
    capitalizeWordsTest ""            []

-- Language exercises
-- 1. Write a function that capitalizes a word
capitalizeWord :: String -> String
capitalizeWord []       = []
capitalizeWord (x : xs) = toUpper x : xs

capitalizeWordTest :: String -> String -> IO ()
capitalizeWordTest input expected = if capitalizeWord input == expected
    then putStrLn
        ("[PASS] input=\"" ++ input ++ "\", expected=" ++ (show expected))
    else putStrLn
        ("[FAIL] input=\"" ++ input ++ "\", expected=" ++ (show expected))

runCapitalizeWordTests :: IO ()
runCapitalizeWordTests = do
    capitalizeWordTest "Chortle" "Chortle"
    capitalizeWordTest "chortle" "Chortle"

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize when
-- a new sentence has begun by checking for periods. Reuse the capitalizeWord
-- function.
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph x  = (intercalate ". " (capitalizeSentences x)) ++ "."  where
    capitalizeSentences x =
        fmap (capitalizeWord . trim) (filter nonEmpty $ splitOn "." x)

nonEmpty :: String -> Bool
nonEmpty x = length x > 0

trim :: String -> String
trim x = dropWhileEnd isSpace (dropWhile isSpace x)

capitalizeParagraphTest input expected =
    if capitalizeParagraph input == expected
        then
            putStrLn
                (  "[PASS] input=\""
                ++ input
                ++ "\", expected="
                ++ (show expected)
                )
        else
            putStrLn
                (  "[FAIL] input=\""
                ++ input
                ++ "\", expected="
                ++ (show expected)
                )

runCapitalizeParagraphTests :: IO ()
runCapitalizeParagraphTests = do
    capitalizeParagraphTest "blah. woot ha." "Blah. Woot ha."
