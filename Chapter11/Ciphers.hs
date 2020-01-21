module Ciphers where
import           Data.Char

toOffset :: Char -> Int
toOffset = (subtract $ ord 'A') . ord . toUpper

fromOffset :: Int -> Char
fromOffset = chr . (+ ord 'A') . (flip mod $ toOffset 'Z' + 1)

encodeCharacter :: Char -> Char -> Char
encodeCharacter code c = fromOffset (toOffset c + toOffset code)

encodeMessage :: String -> String -> String
encodeMessage []      _       = []
encodeMessage _       []      = []
encodeMessage keyword message = go (cycle keyword) message  where
    go :: String -> String -> String
    go _ [] = []
    go keyword@(k : ks) (m : ms) =
        if isSpace m then m : go keyword ms else encodeCharacter k m : go ks ms
