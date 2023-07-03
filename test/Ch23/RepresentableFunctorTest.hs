module Ch23.RepresentableFunctorTest (tests) where

import qualified Lib.Category as C
import Lib.SetCategory
import Ch23.RepresentableFunctor
import Ch23.XCone
import Test.Tasty
import Test.Tasty.HUnit

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
            let f = find A B
                f' = liftH X f

            -- exercise and verify
            assertBool "source" $ (not . null . samples . source) f'
            assertBool "destination" $ (not . null . samples . dest) f',
          --
          testCase "identity" $ do
            let f' = liftH' X (find A B)
                g' = liftH' X (find B A)
                idB = identity $ source f'

            -- exercise and verify
            source f' @?= homSet' B X
            source g' @?= homSet' A X

            f' <.> idB @?= f'
            idB <.> g' @?= g',
          --
          testCase "composition" $ do
            let f = find A B
                f' = liftH' X f
                h = find B C
                h' = liftH' X h

            -- exercise and verify
            source f' @?= homSet' B X
            dest f' @?= homSet' A X

            source h' @?= homSet' C X
            dest h' @?= homSet' B X

            f' <.> h' @?= liftH' X (h C.<.> f)
        ]
    ]
