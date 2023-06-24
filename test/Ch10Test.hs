module Ch10Test (unitTests) where

import Data.Set
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) :: Bool -> Bool -> Bool
a --> b = not a || b

prop_totallyOrdered :: Property
prop_totallyOrdered =
  property $ do
    a <- forAll $ Gen.int (Range.constant 0 10)
    b <- forAll $ Gen.int (Range.constant 0 10)
    c <- forAll $ Gen.int (Range.constant 0 10)

    -- exercise and verify
    H.assert $ a <= a
    H.assert $ (a <= b && b <= a) --> (a == b)
    H.assert $ (a <= b && b <= c) --> (a <= c)
    H.assert $ a <= b || b <= a

prop_powerSet :: Property
prop_powerSet =
  property $ do
    xs <- forAll $ Gen.list (Range.constant 1 10) Gen.alpha
    let xss = (toList . powerSet . fromList) xs

    a <- forAll $ Gen.element (xss)
    b <- forAll $ Gen.element (xss)
    c <- forAll $ Gen.element (xss)

    -- exercise and verify
    H.assert $ a `isSubsetOf` a
    H.assert $ (a `isSubsetOf` b && b `isSubsetOf` a) --> (a == b)
    H.assert $ (a `isSubsetOf` b && b `isSubsetOf` c) --> (a `isSubsetOf` c)

divisors :: Int -> [Int]
divisors x = Prelude.filter ((== 0) . mod x) [1 .. x]

prop_partiallyOrderedTransitive :: Property
prop_partiallyOrderedTransitive =
  property $ do
    n <- forAll $ Gen.int (Range.constant 10 1000)
    a <- forAll $ Gen.element (divisors n)
    b <- forAll $ Gen.element (divisors n)
    c <- forAll $ Gen.element (divisors n)

    -- exercise and verify
    H.assert $ (a `mod` b == 0 && b `mod` c == 0) --> (a `mod` c == 0)

unitTests :: TestTree
unitTests =
  testGroup
    "Chapter 10"
    [ testProperty "totally ordered set" prop_totallyOrdered,
      testProperty "ordering is transitive" prop_partiallyOrderedTransitive,
      testProperty "power set" prop_powerSet
    ]
