module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

roundTripPF = read . show 

main = do 
    print ((roundTrip 4) :: Int)
    print (id 4)
    print ((roundTripPF 4) :: Int)
