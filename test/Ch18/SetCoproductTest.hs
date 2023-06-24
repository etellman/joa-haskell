module Ch18.SetCoproductTest (tests) where

import Assertions.HUnit
import Ch12.SetCategory
import Ch18.SetCoproduct
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "set coproduct" $ do
  -- set up
  let twos = multiplesOf 2
      threes = multiplesOf 3
      sumSet = coproductSet twos threes

      p = SetMorphism twos "p" A sumSet
      q = SetMorphism twos "q" B sumSet

      f = SetMorphism twos "(div 2)" (flip div 2) integers
      g = SetMorphism twos "(div 3)" (flip div 3) integers

  -- exercise
  let k = coproductMorphism f g sumSet

  -- verify
  assertValid k
  k <.> p @?= f
  k <.> q @?= g
