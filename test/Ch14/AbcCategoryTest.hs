module Ch14.AbcCategoryTest (unitTests) where

import Ch08.Category
import Ch14.AbcCategory
import Test.Tasty
import Test.Tasty.HUnit

mabcLabel :: Morphism AbcObject AbcLabel -> String
mabcLabel = abcLabel . mlabel

f :: Morphism AbcObject AbcLabel
f = head $ filter ((== "f") . mabcLabel) morphisms

g :: Morphism AbcObject AbcLabel
g = head $ filter ((== "g") . mabcLabel) morphisms

unitTests :: TestTree
unitTests =
  testGroup
    "ABC Category"
    [ testGroup
        "isomorphism"
        [ testCase "f . g == 1b" $ do
            -- exercise
            let fg = f <.> g

            -- verify
            source fg @?= B
            dest fg @?= B,
          --
          testCase "g . f == 1a" $ do
            -- exercise
            let gf = g <.> f

            -- verify
            source gf @?= A
            dest gf @?= A
        ],
      testCase "A is isomorphic to A" $ do
        let idA = head $ filter ((== "1a") . mabcLabel) morphisms

        -- exercise
        let idAA = idA <.> idA

        -- verify
        source idAA @?= A
        dest idAA @?= A
    ]
