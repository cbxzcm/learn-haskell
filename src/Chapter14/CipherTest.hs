module Chapter14.CipherTest where

import Chapter09.Cipher (fromCaesar, toCaesar)
import Chapter11.Ciphers (fromVigenere, toVigenere)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Arbitrary (arbitrary), Testable (property), forAll, listOf, listOf1)
import Test.QuickCheck.Gen (Gen, elements)

lowercaseLetters :: Gen Char
lowercaseLetters = elements ['a' .. 'z']

safeStrings :: Gen String
safeStrings = listOf lowercaseLetters

nonEmptyStrings :: Gen String
nonEmptyStrings = listOf1 $ elements ['a' .. 'z']

codeSafeStrings :: Gen (String, String)
codeSafeStrings = do
  safeString <- safeStrings
  code <- nonEmptyStrings

  return (code, safeString)

caesarIdentity :: String -> Bool
caesarIdentity s = (fromCaesar . toCaesar) s == s

vigenereIdentity :: (String, String) -> Bool
vigenereIdentity (code, s) = (fromVigenere code . toVigenere code) s == s

main :: IO ()
main = hspec $ do
  describe "Caesar cipher" $ do
    it "results in the original data when decoding encoded data" $ do
      forAll safeStrings caesarIdentity
  describe "Vigenere cipher" $ do
    it "results in the original data when decoding encoded data" $ do
      forAll codeSafeStrings vigenereIdentity