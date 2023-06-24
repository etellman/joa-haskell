module Ch11.MonoidMod4SetTest (unitTests) where

import Ch11.MonoidMod4Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genMonoidMod4 :: Gen MonoidMod4
genMonoidMod4 = MonoidMod4 <$> Gen.int (Range.constant 0 3)

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    x <- forAll $ genMonoidMod4

    -- verify
    x <> mempty === x
    mempty <> x === x

prop_compose :: Property
prop_compose =
  property $ do
    x <- forAll $ genMonoidMod4
    y <- forAll $ genMonoidMod4

    -- exercise
    let actual = x <> y

    -- verify
    actual === (MonoidMod4 $ (m4value x + m4value y) `rem` 4)

prop_group :: Property
prop_group =
  property $ do
    -- set up
    x <- forAll $ genMonoidMod4

    -- exercise
    let negx = MonoidMod4 $ (4 - (m4value x)) `rem` 4

    -- verify
    assert $ m4value negx >= 0 && m4value negx < 4
    x <> negx === mempty
    negx <> x === mempty

unitTests :: TestTree
unitTests =
  testGroup
    "Monoid Mod 4 Addition Set"
    [ testProperty "id . f == f . id == f" prop_identity,
      testProperty "compose" prop_compose,
      testProperty "every morphism has a negative" prop_group
    ]
