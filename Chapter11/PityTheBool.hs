module PityTheBool where

import           Data.Int

-- 1. Given the datatype:

data BigSmall =
    Big Bool |
    Small Bool
    deriving (Eq, Show)

-- What is the cardinality of this datatype? Hint: We already know Bool's
-- cardinality. Show your work as demonstrated earlier.

-- Answer:
-- Big Bool | Small Bool = ??
-- Big Bool + Small Bool == ??
-- 2 + 2 == ??
-- 2 + 2 == 4

-- 2. Given the datatype:
data NumberOrBool =
    Numba Int8 |
    BoolyBool Bool
    deriving (Eq, Show)

-- What is the cardinality of NumberOrBool? What happens if you try to create
-- a Numba with a numeric literal larger than 127? And with a numeric literal 
-- smaller than (-128)?

-- Answer:
-- Numba Int8 | BoolyBool Bool = ??
-- Numba Int8 + BoolyBool Bool == ??
-- 256 + 2 == ??
-- 256 + 2 == 258

largeNumba = Numba 128 -- Numba (-128)
smallNumba = Numba (-129) -- Numba 127
