module Chapter11.Ciphers where

import Data.Char (chr, isSpace, ord)

toOffset :: Char -> Int
toOffset = ord

fromOffset :: Int -> Char
fromOffset = chr

encodeCharacter :: Char -> Char -> Char
encodeCharacter code c = fromOffset (toOffset c + toOffset code)

decodeCharacter :: Char -> Char -> Char
decodeCharacter code c = fromOffset (toOffset c - toOffset code)

toVigenere :: String -> String -> String
toVigenere [] _ = []
toVigenere _ [] = []
toVigenere keyword message = go (cycle keyword) message
  where
    go :: String -> String -> String
    go _ [] = []
    go keyword@(k : ks) (m : ms) =
      if isSpace m then m : go keyword ms else encodeCharacter k m : go ks ms

fromVigenere :: String -> String -> String
fromVigenere [] _ = []
fromVigenere _ [] = []
fromVigenere keyword message = go (cycle keyword) message
  where
    go :: String -> String -> String
    go _ [] = []
    go keyword@(k : ks) (m : ms) =
      if isSpace m then m : go keyword ms else decodeCharacter k m : go ks ms