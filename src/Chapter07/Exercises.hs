module Chapter07.Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = snd d
  where
    xLast = x `divMod` 100
    d = fst xLast `divMod` 10

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b = if b then y else x

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
  | b = y
  | not b = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
