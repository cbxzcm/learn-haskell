module Chapter09.Zipping where

-- 1
myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (a : as) (b : bs) = (a, b) : myZip as bs

-- 2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (a : as) (b : bs) = f a b : myZipWith f as bs

-- 3
myZipRewrite :: [a] -> [b] -> [(a, b)]

myZipReqrite _ [] = []
myZipReqrite [] _ = []

myZipRewrite = myZipWith (,)
