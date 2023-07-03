module Lib.SetCategoryTest (tests) where

import Lib.SetCategory
import Test.Tasty
import Test.Tasty.HUnit

meven :: SetMorphism Int Bool
meven = SetMorphism integers "even" even booleans

modd :: SetMorphism Int Bool
modd = SetMorphism integers "odd" odd booleans

mnot :: SetMorphism Bool Bool
mnot = SetMorphism booleans "not" not booleans

mtimes2 :: SetMorphism Int Int
mtimes2 = SetMorphism integers "(* 2)" (* 2) integers

mplus1 :: SetMorphism Int Int
mplus1 = SetMorphism integers "(+ 1)" (+ 1) integers

tests :: TestTree
tests =
  testGroup
    "Set Category"
    [ testGroup
        "Composition"
        [ testCase "Int -> Int" $ do
            -- exercise
            let actual = mplus1 <.> mtimes2

            -- verify
            actual @?= SetMorphism integers "2x + 1" (\x -> 2 * x + 1) integers,
          --
          testCase "Int -> Bool" $ mnot <.> meven @?= modd
        ],
      -- testCase "show morphism" $ show meven @?= "even :: Int -> Bool",
      testCase "not equal" $ assertBool "unequal morphisms" (meven /= modd),
      testGroup
        "unit law"
        [ testCase "id . f" $ identity integers <.> mplus1 @?= mplus1,
          testCase "f . id" $ mplus1 <.> identity integers @?= mplus1
        ],
      testCase "associativity law" $ do
        -- set up
        let mplus3 = SetMorphism integers "(+ 3)" (+ 3) integers

        -- exercise and verify
        (mplus1 <.> mplus1) <.> mplus1 @?= mplus3
        mplus1 <.> (mplus1 <.> mplus1) @?= mplus3
    ]
