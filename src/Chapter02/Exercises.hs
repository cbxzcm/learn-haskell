module Chapter02.Exercises where

z = 7

x = y ^ 2

waxOn = x * y
  where
    y = 5

y = z + 8

triple x = x * 3

waxOff :: Fractional a => a -> a
waxOff x = triple x / 10
