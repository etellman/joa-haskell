module Ch23.RepresentableFunctorTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.RepresentableFunctor
import Ch23.XCone
import Test.Tasty
import Test.Tasty.HUnit

find :: XConeObject -> XConeObject -> C.Morphism XConeObject SumLabel
find = C.findIn C.morphisms

find' :: XConeObject -> XConeObject -> C.Morphism XConeObject SumLabel
find' = C.findIn C.opposite

tests :: TestTree
tests =
  testGroup
    "Representable Functors"
    [ testGroup
        "Hx"
        [ testCase "source and destination" $ do
            let f = find A B
                f' = liftH X f

            -- exercise and verify
            assertBool "source" $ (not . null . samples . source) f'
            assertBool "destination" $ (not . null . samples . dest) f',
          testCase "identity" $ do
            let f' = liftH X (find A B)
                g' = liftH X (find B A)
                idA = identity $ source f'

            -- exercise and verify
            f' <.> idA @?= f'
            idA <.> g' @?= g',
          --
          testCase "composition" $ do
            let f = find A B
                f' = liftH X f
                h = find B C
                h' = liftH X h

            -- exercise and verify
            h' <.> f' @?= liftH X (h C.<.> f)
        ],
      testGroup
        "Hx dual"
        [ testCase "source" $ do
            let f = find' B A
                f' = liftH X f

            -- exercise and verify
            assertBool "source" $ (not . null . samples . source) f'
            assertBool "destination" $ (not . null . samples . dest) f',
          --
          testCase "identity" $ do
            let f' = liftH' X (find' B A)
                g' = liftH' X (find' A B)
                idA = identity $ source g'

            -- exercise and verify
            f' <.> idA @?= f'
            idA <.> g' @?= g',
          --
          testCase "composition" $ do
            let f = find' B A
                f' = liftH' X f
                h = find' C B
                h' = liftH' X h

             -- exercise and verify
            h' <.> f' @?= liftH' X (f C.<.> h)
        ]
    ]
