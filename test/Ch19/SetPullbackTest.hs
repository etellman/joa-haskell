module Ch19.SetPullbackTest (tests) where

import qualified Assertions.HUnit as UA
import qualified Assertions.Hedgehog as HA
import Lib.SetCategory
import Ch18.Product
import Ch19.SetPullback
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_favoriteSquare :: Property
prop_favoriteSquare = property $ do
  a <- forAll $ Gen.int (Range.constant 2 8)
  b <- forAll $ Gen.int (Range.constant 2 8)
  xs <- forAll $ Gen.list (Range.constant 5 20) (Gen.int $ Range.constant 0 100)

  let isMultiple x k = x `mod` k == 0
      aSet = finiteSet "A" (filter (flip isMultiple a) xs)
      bSet = finiteSet "B" (filter (flip isMultiple b) xs)
      intersectSet = finiteSet "A ^ B" (filter (\x -> x `isMultiple` a && x `isMultiple` b) xs)
      unionSet = finiteSet "A v B" xs
      isEmpty = null . samples

  H.cover 5 "a ^ b empty" $ isEmpty intersectSet
  H.cover 30 "a ^ b not empty" $ (not . isEmpty) intersectSet

  H.cover 5 "a == b" $ samples aSet == samples bSet
  H.cover 30 "a /= b" $ samples aSet /= samples bSet

  intersectionToA <- HA.assertValid $ SetMorphism intersectSet "intersectionToA" id aSet
  intersectionToB <- HA.assertValid $ SetMorphism intersectSet "intersectionToB" id bSet
  aToUnion <- HA.assertValid $ SetMorphism aSet "aToUnion" id unionSet
  bToUnion <- HA.assertValid $ SetMorphism bSet "bToUnion" id unionSet

  -- exercise and verify
  aToUnion <.> intersectionToA === bToUnion <.> intersectionToB

-- | A set pullback that finds all the multiples of 2 or 3 that are the same mod 5 or 7
tests :: TestTree
tests =
  testGroup
    "Pullback"
    [ testCase "set pullback" $ do
        -- set up
        let xSet =
              let isValid x = 2 * x `mod` 5 == 3 * x `mod` 7
               in SetObject "X" (filter isValid [-100 .. 100]) isValid

            aSet = multiplesOf 2
            bSet = multiplesOf 3
            cSet = SetObject "Rem7" [0 .. 6] (flip elem [0 .. 6])

            s = SetMorphism aSet "mod 5" (flip mod 5) cSet
            t = SetMorphism bSet "mod 7" (flip mod 7) cSet

            pairs = pullbackSet aSet bSet s t

            f = SetMorphism xSet "(* 2)" (* 2) aSet
            p = SetMorphism pairs "fst" fst aSet

            g = SetMorphism xSet "(* 3)" (* 3) bSet
            q = SetMorphism pairs "snd" snd bSet

        -- exercise
        let k = productMorphism f g pairs
        UA.assertValid k

        -- verify
        s <.> p @?= t <.> q
        p <.> k @?= f
        q <.> k @?= g
        t <.> q <.> k @?= t <.> g
        s <.> p <.> k @?= s <.> f,
      --
      testProperty "overlap" prop_favoriteSquare
    ]
