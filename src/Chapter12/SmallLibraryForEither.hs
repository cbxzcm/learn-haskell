module Chapter12.SmallLibraryForEither where

lefts' :: [Either a b] -> [a]
lefts' = foldr foldLefts []
  where
    foldLefts :: Either a b -> [a] -> [a]
    foldLefts (Left x) b = x : b
    foldLefts _ b = b

rights' :: [Either a b] -> [b]
rights' = foldr foldRights []
  where
    foldRights :: Either a b -> [b] -> [b]
    foldRights (Right x) b = x : b
    foldRights _ b = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr partition ([], [])
  where
    partition :: Either a b -> ([a], [b]) -> ([a], [b])
    partition (Left x) (lefts, rights) = (x : lefts, rights)
    partition (Right x) (lefts, rights) = (lefts, x : rights)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

-- TODO Is this possible?
-- eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
-- eitherMaybe'' _ (Left _) = Nothing
-- eitherMaybe'' f e = Just $ either' id f e
