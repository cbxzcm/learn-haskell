module Chapter09.TheFearfulSymmetry where

myWords :: String -> [String]
myWords x = go [] x
  where
    go results x
      | null x = results
      | otherwise = go (results ++ [takeWhile (/= ' ') x]) (dropWhile (== ' ') $ dropWhile (/= ' ') x)

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x = go [] x
  where
    go results x
      | null x = results
      | otherwise = go (results ++ [takeWhile (/= '\n') x]) (dropWhile (== '\n') $ dropWhile (/= '\n') x)

mySplit :: Char -> String -> [String]
mySplit splitChar x = go [] x
  where
    go results x
      | null x = results
      | otherwise = go (results ++ [takeWhile (/= splitChar) x]) (dropWhile (== splitChar) $ dropWhile (/= splitChar) x)