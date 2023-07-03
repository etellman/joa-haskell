module Ch20.FunctorLawsTest (tests) where

import Lib.SetCategory
import Lib.SetFunctor
import Functors.FFunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_identityNative :: Property
prop_identityNative = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant 0 100)

  -- exercise and verify
  F (id x) === fmap id (F x)

prop_morphNative :: Property
prop_morphNative = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant 0 100)

  -- exercise and verify
  fmap (+ 1) (F x) === F (x + 1)

prop_identity :: Property
prop_identity = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  let fid = lift $ identity integers :: SetMorphism (F Int) (F Int)

  -- exercise and verify
  (op fid $ F x) === F x

prop_morph :: Property
prop_morph = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)

  let f = multiply x
      ff = lift f :: SetMorphism (F Int) (F Int)

  -- exercise and verify
  (op ff $ F y) === (F $ op f y)

tests :: TestTree
tests =
  testGroup
    "Functor Laws"
    [ testGroup
        "Native Haskell"
        [ testProperty "identity" prop_identityNative,
          testProperty "morphism" prop_morphNative
        ],
      testGroup
        "Set Category"
        [ testProperty "identity" prop_identity,
          testProperty "morphism" prop_morph
        ]
    ]
