module Ch20.CharacterCodeTest (tests) where

import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

-- word encoding, p. 301
-- lift a function that operates on characters to one that operates on strings, since
-- strings are the free monoid for concatenating characters

encode :: Int -> Char -> Char
encode offset = chr . (+ offset) . ord

hide :: Char -> Char
hide = const '*'

prop_encode :: Property
prop_encode = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 1 20)
  word <- forAll $ Gen.string (Range.constant 0 10) Gen.lower

  -- exercise
  let encoded = fmap (encode n) word

  -- verify
  fmap (ord) encoded === fmap ((+ n) . ord) word

prop_roundTrip :: Property
prop_roundTrip = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 1 20)
  word <- forAll $ Gen.string (Range.constant 0 10) Gen.lower

  let encoder = fmap (encode n)
      decoder x = Just $ fmap (encode $ negate n) x

  -- exercise and verify
  tripping word encoder decoder

tests :: TestTree
tests =
  testGroup
    "Encode characters"
    [ testCase "encode single character" $
        encode 3 'a' @?= 'd',
      testCase "encode word" $
        fmap (encode 2) "abcd" @?= ("cdef" :: String),
      testProperty "encode" prop_encode,
      testProperty "round trip" prop_roundTrip,
      testCase "hide word" $
        fmap hide "abcd" @?= ("****" :: String)
    ]
