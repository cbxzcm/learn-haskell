module Chapter11.DogTypes where

data Trivial = Trivial'
data UnaryTypeCon a = UnaryTYpeCon a
data PugType = PugData
data HuskyType = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge
data Doggies a =
    Husky a |
    Mastiff a
    deriving (Eq, Show)

-- 1. Is Doggies a type constructor or a data constructor?
-- Answer: Type constructor

-- 2. What is the kind of Doggies?
-- Answer: * -> * (use :kind Doggies)

-- 3. WHat is the kind of Doggies String?
-- Answer: * (use :kind Doggies String)

-- 4. What is the type of Husky 10?
-- Answer: Husky 10 :: Num a => Doggies a (use :type Husky 10)

-- 5. What is the type of Husky (10 :: Integer)?
-- Answer: Husky (10 :: Integer) :: Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?
-- Answer: Mastiff "Scooby Doo" :: Doggies [Char]

-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
-- Answer: It's the name of the type and data constructor

-- 8. What is the type of DogueDeBordeaux 
-- Answer: DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 9. What is the type of DogueDeBordeaux "doggie!"
-- Answer: DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]



