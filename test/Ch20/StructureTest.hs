module Ch20.StructureTest (tests) where

import Ch12.SetCategory
import Test.Tasty
import Test.Tasty.HUnit

f :: SetMorphism Int Int
f = SetMorphism integers "(* 2)" (* 2) (multiplesOf 2)

g :: SetMorphism Int Int
g = SetMorphism integers "(* 4)" (* 4) (multiplesOf 2)

fObject :: SetObject Int -> SetObject ()
fObject _ = singleton

fMorphism ::
  SetMorphism Int Int ->
  SetMorphism () ()
fMorphism (SetMorphism _ _ _ _) = identity singleton

tests :: TestTree
tests =
  testGroup
    "Reflection - p. 303"
    [
      testCase "the dest object is always the same" $ do
        -- exercise
        let ints = fObject integers
            twos = fObject (multiplesOf 2)

        -- verify
        ints @?= singleton
        twos @?= singleton,
    testCase "f not isomorphism, Ff isomorphism" $ do
        fMorphism f <.> fMorphism f @?= identity singleton,
      testCase "f and g don't commute, f' and g' do commute" $ do
        assertBool "original" $ f /= g
        assertBool "transformed" $ fMorphism f == fMorphism g
    ]
