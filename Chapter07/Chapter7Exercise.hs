module Chapter7Exercise where

tensDigit :: Integral a => a -> a
tensDigit x = snd d
	where
		xLast = x `divMod` 100
		d = (fst xLast) `divMod` 10

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =
    case b of 
        True -> y
        False -> x

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b 
    | b == True = y
    | b == False = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
