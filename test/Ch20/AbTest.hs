module Ch20.AbTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

fa :: String -> String
fa = ("a" ++)

fb :: String -> String
fb = ("b" ++)

tests :: TestTree
tests =
  testGroup
    "Words with 'a' and 'b'"
    [ testCase "compose 1" $
        do
          -- exercise
          let ab = fa . fb

          -- verify
          ab "" @?= "ab",
      testCase "compose 2" $
        do
          -- exercise
          let f = fa . fb . fa . fb . fb

          -- verify
          f "" @?= "ababb"
    ]
