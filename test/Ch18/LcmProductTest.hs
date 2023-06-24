module Ch18.LcmProductTest (tests) where

import Ch08.Category
import Ch08.Factor30
import Ch18.LcmProduct
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

prop_withdest :: Property
prop_withdest =
  property $ do
    x <- forAll $ Gen.element (objects :: [Factor30])
    let match = morphismsTo :: Factor30 -> [Morphism Factor30 Factor30Label]

    -- exercise
    let ms = match x

    -- verify
    let sourceMultipleOfX m = (f30factor . source) m `mod` f30factor x == 0
    assert $ all sourceMultipleOfX ms

prop_commonSource :: Property
prop_commonSource =
  property $ do
    x <- forAll $ Gen.element (objects :: [Factor30])
    y <- forAll $ Gen.element (objects :: [Factor30])

    -- exercise
    let ms = commonSource x y

    -- verify
    let isMultiple z m = f30factor m `mod` f30factor z == 0
    assert $ all (isMultiple x) ms && all (isMultiple y) ms

prop_minCommonSource :: Property
prop_minCommonSource =
  property $ do
    x <- forAll $ Gen.element (objects :: [Factor30])
    y <- forAll $ Gen.element (objects :: [Factor30])

    -- exercise and verify
    f30factor (minCommonSource x y) === lcm (f30factor x) (f30factor y)

tests :: TestTree
tests =
  testGroup
    "LCM"
    [ testProperty "with dest" prop_withdest,
      testProperty "common source" prop_commonSource,
      testProperty "LCM product" prop_minCommonSource
    ]
