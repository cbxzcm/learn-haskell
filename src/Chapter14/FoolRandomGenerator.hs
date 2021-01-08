module Chapter14.FoolRandomGenerator where

import Test.QuickCheck (Gen, frequency, oneof, sample')

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

foolGenEqualProb :: Gen Fool
foolGenEqualProb = oneof [return Fulse, return Frue]

foolGenUnequalProb :: Gen Fool
foolGenUnequalProb = frequency [(2, return Fulse), (1, return Frue)]

main :: IO ()
main = do
  sampleEqualProb <- sample' foolGenEqualProb
  sampleUnequalProb <- sample' foolGenUnequalProb

  putStrLn $ "Equal probabiilty: " ++ show sampleEqualProb
  putStrLn $ "2/3 Fulse probability: " ++ show sampleUnequalProb
