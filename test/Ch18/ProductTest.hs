module Ch18.ProductTest (tests) where

import Assertions.HUnit
import Ch12.SetCategory
import Ch18.Product
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "set product" $ do
  -- set up
  let prod = productSet (multiplesOf 2) (multiplesOf 3)
      f = SetMorphism integers "(* 2)" (* 2) (multiplesOf 2)
      p = SetMorphism prod "fst" fst (multiplesOf 2)

      g = SetMorphism integers "(* 3)" (* 3) (multiplesOf 3)
      q = SetMorphism prod "snd" snd (multiplesOf 3)

  -- exercise
  let k = productMorphism f g prod

  -- verify
  assertValid k
  p <.> k @?= f
  q <.> k @?= g
