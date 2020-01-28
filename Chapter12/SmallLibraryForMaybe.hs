module SmallLibraryForMaybe where

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing  = b
mayybe b f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe b = maybe b id

listToMaybe :: [a] -> Maybe a
listToMaybe []         = Nothing
listToMaybe (head : _) = Just head

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes (x : xs) = case x of
    Nothing -> catMaybes xs
    Just x  -> x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr flipMaybe' (Just [])  where
    flipMaybe' :: Maybe a -> Maybe [a] -> Maybe [a]
    flipMaybe' Nothing  _        = Nothing
    flipMaybe' _        Nothing  = Nothing
    flipMaybe' (Just x) (Just l) = Just (x : l)
