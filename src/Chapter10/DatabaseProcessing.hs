module Chapter10.DatabaseProcessing where

import           Data.Time
import           Data.List

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map getUTC . filter isDbDate

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate a) = True
isDbDate _          = False

getUTC :: DatabaseItem -> UTCTime
getUTC (DbDate x) = x

-- 2. Write a function that filters for DbNumber values and returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map getDbNum . filter isDbNumber

getDbNum :: DatabaseItem -> Integer
getDbNum (DbNumber x) = x

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

-- 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent x = foldr (\a b -> a) undefined (take 1 $ sort $ filterDbDate x)

-- 4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb x = foldr (\a b -> a + b) 0 (filterDbNumber x)

-- 5. Write a function that gets the average of the DbNumber values.
avgDb :: [DatabaseItem] -> Double
avgDb x = (fromIntegral $ sumDb x) / (fromIntegral $ length x)
