module Chapter09.MyHead where

myHead :: [a] -> Maybe [a]
myHead [] = Nothing
myHead (x : _) = Just [x]
