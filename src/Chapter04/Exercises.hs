module Chapter04.Exercises where

awesome = ["Papuchon", "curry", ":)"] 
also = ["Quake", "The Simons"] 
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = 
  if x < 0 then
    x * (-1)
  else
    x

x = (+)

f xs = x w 1 
  where w = length xs
