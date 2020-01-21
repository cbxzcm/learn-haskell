module Vehicles where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini |
    Mazda |
    Tata
    deriving (Eq, Show)

data Airline =
    PapuAir |
    CatapultsR'Us |
    TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price |
    Plane Airline Size
    deriving (Eq, Show)

data Size =
    Small |
    Medium |
    Large
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Medium

-- 1. WHat is the type of myCar?
-- Answer: myCar :: Vehicle

-- 2. Given the following, define the functions:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars []            = []
areCars (head : tail) = (isCar head) : (areCars tail)

-- 3. Now we're going to write a function to tell us the manufacturer of a piece of data:
getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer
getManu _                    = undefined

-- 4. Given that we're returning the Manufacturer, what will happen if you use this on Plane data?
-- Answer: You get an undefined exception

-- 5.All right. Let's say you've decided to add the size of the plane as an argument to the Plane
-- constructor. Add that to your datatypes in the appropriate places and change your data and
-- functions appropriately.
--
-- Answer: Done