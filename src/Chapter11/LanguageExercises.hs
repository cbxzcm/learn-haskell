module Chapter11.LanguageExercises where
import           Data.Char
import           Data.List.Split
import           Data.List

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
