module Ch22.NaturalTransformationTest (tests) where

import Assertions.Hedgehog
import Lib.SetCategory
import Ch20.SetFunctor
import Functors.FFunctor
import Functors.GFunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_naturalTransformation :: Property
prop_naturalTransformation = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 1000)

  let f = lift $ multiply n :: SetMorphism (F Int) (F Int)
      g = lift $ multiply n :: SetMorphism (G Int) (G Int)

  alphaX <- assertValid $ natural (source f)
  alphaY <- assertValid $ natural (dest f)

  -- exercise and verify
  g <.> alphaX === alphaY <.> f

tests :: TestTree
tests = testProperty "Natural Transformation" prop_naturalTransformation
