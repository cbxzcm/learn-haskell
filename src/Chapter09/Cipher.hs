module Chapter09.Cipher where

import Data.Char

toCaesar :: String -> String
toCaesar = fmap encodeCaesar

fromCaesar :: String -> String
fromCaesar = fmap decodeCaesar

toBase :: Char -> Int
toBase = (subtract $ ord 'a') . ord . toLower

fromBase :: Int -> Char
fromBase = chr . (+ord 'a')

encodeCaesar :: Char -> Char
encodeCaesar x = fromBase $ mod (3 + toBase x) 26

decodeCaesar :: Char -> Char
decodeCaesar x = fromBase $ mod (subtract 3 (toBase x)) 26
