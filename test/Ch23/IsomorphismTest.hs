module Ch23.IsomorphismTest (tests) where

import Ch12.SetCategory
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_baseIsomorphism :: Property
prop_baseIsomorphism = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 1 200)

  let f = multiply n
      g = divide n

  -- exercise and verify
  f <.> g === (identity $ multiplesOf n)
  g <.> f === (identity $ integers)

tests :: TestTree
tests =
  testGroup
    "Isomorphism"
    [ testProperty "base isomorphism" prop_baseIsomorphism
    ]
