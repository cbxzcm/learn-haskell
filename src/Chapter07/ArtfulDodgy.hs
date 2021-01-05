module Chapter07.ArtfulDodgy where

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = flip dodgy 2
