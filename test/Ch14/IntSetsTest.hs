module Ch14.IntSetsTest (unitTests) where

import Lib.SetCategory
import Ch14.IntSets
import Test.Tasty
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests =
  testGroup
    "Isomorphisms With Sets"
    [ testGroup
        "Int2 is isomorphic to Int4 - p. 169-71"
        [ testCase "Int2 -> Int4 -> Int2" $ f42 <.> f24 @?= identity (multiplesOf 2),
          testCase "Int4 -> Int2 -> Int4" $ f24 <.> f42 @?= identity (multiplesOf 4)
        ],
      testGroup
        "Int2 and Int4 have the same relationships with Int - p. 172"
        [ testGroup
            "Int to/from Int4 through Int2"
            [ testCase "Int -> Int2 -> Int4" $ f24 <.> f12 @?= f14,
              testCase "Int4 -> Int2 -> Int" $ f21 <.> f42 @?= f41
            ],
          testGroup
            "Int to/from Int2 through Int4"
            [ testCase "Int -> Int4 -> Int2" $ f42 <.> f14 @?= f12,
              testCase "Int2 -> Int4 -> Int" $ f41 <.> f24 @?= f21
            ]
        ]
    ]
