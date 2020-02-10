module Chapter09.MyStandardFunctions where

-- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem item (x:xs) = item == x || myElem item xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny item xs = any (\x -> x == item) xs

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs 

-- 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7 
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy f (a:[]) = a 
myMaximumBy f (a:b:xs) 
    | f a b == LT = myMaximumBy f (b:xs)
    | otherwise = myMaximumBy f (a:xs)

-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f (a:[]) = a 
myMinimumBy f (a:b:xs) 
    | f a b == LT = myMinimumBy f (a:xs)
    | otherwise = myMinimumBy f (b:xs)

-- 10
myMaximum :: (Ord a) => [a] -> a
myMaximum a = myMaximumBy compare a

myMinimum :: (Ord a) => [a] -> a
myMinimum a = myMinimumBy compare a
