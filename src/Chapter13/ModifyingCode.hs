module Chapter13.ModifyingCode where

import           Data.Char
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           System.Exit                    ( exitSuccess )
import           Control.Monad                  ( forever )
import           Chapter11.Ciphers              ( toVigenere )
import           Chapter09.Cipher               ( toCaesar )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )

runCaesar :: IO ()
runCaesar = forever $ do
    hSetBuffering stdout NoBuffering
    putStr "Enter a message: "
    message <- getLine

    putStrLn $ "Encoded message: " ++ toCaesar message

runVigenere :: IO ()
runVigenere = forever $ do
    hSetBuffering stdout NoBuffering
    putStr "Enter a message: "
    message <- getLine

    putStr "Enter a keyword: "
    keyword <- getLine

    putStrLn $ "Encoded message: " ++ toVigenere keyword message

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine

    let normalizedLine = normalize line1
    if normalizedLine == reverse normalizedLine
        then putStrLn "It's a palindronme!"
        else do
            putStrLn "Nope!"
            exitSuccess
  where
    foldFunc :: Char -> String -> String
    foldFunc c s = case (isSpace c, isLetter c) of
        (True, _    ) -> s
        (_   , False) -> s
        (_   , _    ) -> toLower c : s

    normalize :: String -> String
    normalize = foldr foldFunc []

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty |
    AgeTooLow |
    PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0
    = Right $ Person name age
    | name == ""
    = Left NameEmpty
    | age <= 0
    = Left AgeTooLow
    | otherwise
    = Left
        $  PersonInvalidUnknown
        $  "Name was: "
        ++ show name
        ++ " Age was: "
        ++ show age

gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Enter name: "
    name <- getLine
    putStr "Enter Age: "
    ageStr <- getLine

    let age = read ageStr :: Integer

    putStrLn $ printParsePersonResult $ mkPerson name age

printParsePersonResult :: Either PersonInvalid Person -> String
printParsePersonResult (Left NameEmpty) = "An error occurred: empty name"
printParsePersonResult (Left AgeTooLow) = "An error occurred: age too low"
printParsePersonResult (Left (PersonInvalidUnknown message)) =
    "An error occurred: " ++ message
printParsePersonResult (Right person) =
    "Yay! Successfully got a Person: " ++ show person





