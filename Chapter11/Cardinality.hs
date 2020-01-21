module Cardinality where

-- What is the cardinality of these types:

-- 1
data PugType = PugData

-- Answer: 1

-- 2. For this one, recall that Bool is also defined with the |:
data Airline =
    PapuAir |
    CatapultsR'Us |
    TakeYourChancesUnited

-- Answer: 3

-- 3. Given what we know about Int8, what's the cardinality of Int16?
-- Answer: 2^16 = 65536

-- 4. Use the REPL and maxBound and minBound to examine Int and Integer.
--    What can you say about the cardinality of those types?

-- -9223372036854775808
--minBound :: Int 

-- 9223372036854775807
--maxBound :: Int 

--minBound :: Integer -- No instance for (Bounded Integer
--maxBound :: Integer -- No instance for (Bounded Integer)

-- 5. Extra credit (impress your friends!): What's the connection between the
--    8 in Int8 and the type's cardinality of 256?
-- Answer: 2^8 = 256
