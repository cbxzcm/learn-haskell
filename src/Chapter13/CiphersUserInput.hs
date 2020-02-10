module Chapter13.CiphersUserInput where

import           Data.Char
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           Control.Monad                  ( forever )
import           Chapter11.Ciphers              ( toVigenere )
import           Chapter09.Cipher               ( toCaesar )

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

