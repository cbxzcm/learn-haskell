module Chapter03.Reverse where

rvrs :: String -> String
rvrs input = c ++ " " ++ b ++ " " ++ a
  where
    a = take 5 input
    b = take 2 (drop 6 input)
    c = drop 9 input

main :: IO ()
main = print (rvrs "Curry is awesome!")
