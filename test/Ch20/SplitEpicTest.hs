module Ch20.SplitEpicTest (tests) where

import Assertions.Hedgehog
import Ch12.SetCategory
import Ch20.SetFunctor
import Functors.FFunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.Printf

add :: Int -> SetMorphism (F Int) (F Int)
add n = SetMorphism intFs (printf "(+ %d)" n) (create . (+ n) . extract) intFs

prop_splitEpic :: Property
prop_splitEpic = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant 1 100)
  y <- forAll $ Gen.element [x, -x]

  cover 30 "split epic" $ x + y == 0
  cover 30 "not split epic" $ x + y /= 0

  -- exercise and verify
  (x + y == 0) <==> (add x <.> add y == identity intFs)

tests :: TestTree
tests = testProperty "Split Epic" prop_splitEpic
