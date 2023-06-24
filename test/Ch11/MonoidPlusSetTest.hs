module Ch11.MonoidPlusSetTest (unitTests) where

import Ch11.MonoidPlusSet
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genMonoidPlus :: Gen MonoidPlus
genMonoidPlus = MonoidPlus <$> Gen.int (Range.constant 0 1000)

prop_identity :: Property
prop_identity =
  property $ do
    x <- forAll $ genMonoidPlus

    -- exercise and verify
    mempty <> x === x
    x <> mempty === x

prop_compose :: Property
prop_compose =
  property $ do
    x <- forAll $ genMonoidPlus
    y <- forAll $ genMonoidPlus

    -- exercise
    let actual = x <> y

    -- verify
    actual === MonoidPlus (mpvalue x + mpvalue y)

prop_group :: Property
prop_group =
  property $ do
    -- set up
    x <- forAll $ genMonoidPlus

    -- exercise
    let negx = MonoidPlus $ (negate . mpvalue) x

    -- verify
    x <> negx === mempty
    negx <> x === mempty

unitTests :: TestTree
unitTests =
  testGroup
    "Monoid Addition Set"
    [ testProperty "id . f == id . f == f" prop_identity,
      testProperty "compose" prop_compose,
      testProperty "every morphism has a negative" prop_group
    ]
