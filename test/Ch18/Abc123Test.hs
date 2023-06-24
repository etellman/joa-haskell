module Ch18.Abc123Test (tests) where

import Ch12.SetCategory
import Ch18.Product
import Test.Tasty
import Test.Tasty.HUnit
import Assertions.HUnit

circularSelect :: [a] -> Int -> a
circularSelect xs n = xs !! (n `rem` (length xs))

aSet :: SetObject Char
aSet = finiteSet "A" ['a', 'b']

bSet :: SetObject Int
bSet = finiteSet "B" [1, 2, 3]

cSet :: SetObject Int
cSet = finiteSet "C" [1 .. 6]

tests :: TestTree
tests = testCase "ABC/123 product - p. 247-48" $ do
  -- set up
  let axb = productSet aSet bSet
      p = SetMorphism axb "p" fst aSet
      pj = SetMorphism cSet "pj" (circularSelect $ samples aSet) aSet

      q = SetMorphism axb "q" snd bSet
      qj =  SetMorphism cSet "qj" (circularSelect $ samples bSet) bSet

  -- exercise
  let j = productMorphism pj qj axb

  -- verify
  assertValid j
  p <.> j @?= pj
  q <.> j @?= qj
