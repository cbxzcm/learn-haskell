module Chapter11.ForExample where

data Example = MakeExample deriving (Show)

-- 1. WHat is the type of data constructor MakeExample? What happens when you request  the type of Example?
--
-- Answer: The type of MakeExample is Example.
--

-- * Chapter11ForExample> :t MakeExample

-- MakeExample :: Example

-- 2. What if you try :info on Example in GHCi? Can you determine what type class instances are defined for
--    the Example type using :info in GHCi?
--
-- Answer: Using :info shows that Example has a typeclass instance for Show.
--

-- * Chapter11ForExample> :info Example

-- data Example = MakeExample
--  	-- Defined at chapter11-for-example.hs:3:1
-- instance [safe] Show Example
--   -- Defined at chapter11-for-example.hs:3:37

-- 3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as
-- Int. What has changed when you query MakeExample with :type in GHCIi?
newtype MyExample = MakeMyExample Int deriving (Show)

-- Answer: Using :type MakeMyExample shows that the type is Int -> MyExample
--

-- * Chapter11ForExample> :t MakeMyExample

-- MakeMyExample :: Int -> MyExample
