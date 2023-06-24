module Ch06.DistanceTest (unitTests) where

import Ch06.Distance
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_nonNegative :: Property
prop_nonNegative =
  property $ do
    x <- forAll $ Gen.int (Range.constant 0 100)
    y <- forAll $ Gen.int (Range.constant 0 100)

    -- exercise
    let actual = distance x y

    -- verify
    H.assert $ actual >= 0

prop_zero :: Property
prop_zero =
  property $ do
    x <- forAll $ Gen.int (Range.constant 0 10)
    y <- forAll $ Gen.int (Range.constant 0 10)

    -- exercise
    let actual = distance x y

    -- verify
    H.assert $ x /= y || actual == 0

prop_symmetry :: Property
prop_symmetry =
  property $ do
    x <- forAll $ Gen.int (Range.constant 0 100)
    y <- forAll $ Gen.int (Range.constant 0 100)

    -- exercise and verify
    (distance x y) === (distance y x)

prop_detour :: Property
prop_detour =
  property $ do
    x <- forAll $ Gen.int (Range.constant 0 100)
    y <- forAll $ Gen.int (Range.constant 0 100)
    z <- forAll $ Gen.int (Range.constant 0 100)

    -- exercise and verify
    H.assert $ (distance x y) + (distance y z) >= distance x z

unitTests :: TestTree
unitTests =
  testGroup
    "Distance"
    [
      testProperty "non-negative" prop_nonNegative,
      testProperty "zero implies equality" prop_zero,
      testProperty "symmetry" prop_symmetry,
      testProperty "detour" prop_detour
    ]
