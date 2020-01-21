module Chapter3Exercise where

exclamation :: String -> String
exclamation x = x ++ "!"

get4 :: String -> Char 
get4 x = x !! 4

drop10 :: String -> String
drop10 x = drop 10 x

thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String
rvrs = c ++ " " ++ b ++ " " ++ a
  where
    input = "Curry is awesome!"
    a = take 5 input
    b = take 2 (drop 6 input)
    c = drop 9 input
