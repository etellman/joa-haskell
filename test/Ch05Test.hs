module Ch05Test (unitTests) where

import Ch05
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_mod4add :: Property
prop_mod4add =
  property $ do
    x <- forAll $ Gen.int (Range.constant 0 100)
    y <- forAll $ Gen.int (Range.constant 0 100)

    -- exercise
    let actual = mod4add x y

    -- verify
    H.assert $ actual < 4
    H.assert $ actual >= 0

-- | Multiply the factors of 30 in any order to get 30
prop_30 :: Property
prop_30 =
  property $ do
    xs <- forAll $ Gen.shuffle ([2, 3, 5] :: [Int])
    let morphisms = fmap (*) xs

    -- exercise
    let actual =  foldr (.) id morphisms 1

    -- verify
    actual === 30

unitTests :: TestTree
unitTests =
  testGroup
    "Chapter 5"
    [ testGroup
        "mod 4 addition"
        [ testProperty "the result is always between 0 and 3" prop_mod4add,
          testCase "0 + 0" $ mod4add 0 0 @?= 0,
          testCase "2 + 3" $ mod4add 2 3 @?= 1,
          testCase "3 + 3" $ mod4add 3 3 @?= 2
        ],
      testProperty "multiply factors in any order" prop_30
    ]
