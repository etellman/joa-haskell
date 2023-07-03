module Ch15.IntSetMonicEpicTest (unitTests) where

import Lib.SetCategory
import Test.Tasty
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests =
  testGroup
    "Monics and Epics with Integer Sets -- p. 186 - 199"
    [ testCase "not monic" $ do
        -- set up
        let s = SetMorphism integers "id" id integers
        let t = SetMorphism integers "negate" negate integers
        let f = SetMorphism integers "square" (^ (2 :: Int)) integers

        -- exercise and verify
        f <.> s @?= f <.> t,
      --
      testCase "not epic" $ do
        -- set up
        let f = SetMorphism (multiplesOf 2) "id" id integers
            s =
              let negateOdds x = if odd x then -x else x
               in SetMorphism integers "negateOdds" negateOdds integers
            t = SetMorphism integers "id" id integers

        -- exercise and verify
        s <.> f @?= t <.> f
    ]
