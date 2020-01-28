module WriteYourOwnIterateAndUnfoldr where

myIterate :: (a -> a) -> a -> [a]
myIterate f b = b : myIterate f (f b)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
    Nothing     -> []
    Just (x, y) -> x : myUnfoldr f y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))
