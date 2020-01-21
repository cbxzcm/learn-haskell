module RewritingFunctionsUsingFolds where

-- 1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny returns Ture if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

-- 3. Write two versions of myElem. One version should use the folding and the other should use any.
myElemFold :: Eq a => a -> [a] -> Bool
myElemFold target list = foldr (\a b -> a == target || b) False list

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny target list = any (\x -> x == target) list

-- 4. Implement myReverse, don't worry about trying to make it lazy
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5. Write myMap in terms of foldr. It should have the same behavior as the buld-in map
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6. Write myFilter in terms of foldr. It should have the same behavior as the built-in filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

-- 7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

-- 8. squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

-- 9. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy takes a parison function and a list and returns the greatest element of the 
-- list based on the last value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldl (\a b -> if f a b == GT then a else b) x xs

-- 11. myMinimumBY takes a comparison function and list and returns the least element of the
-- list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = foldl (\a b -> if f a b == LT then a else b) x xs
