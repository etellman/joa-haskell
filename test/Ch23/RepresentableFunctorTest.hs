module Ch23.RepresentableFunctorTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.RepresentableFunctor
import Ch23.XCone
import Ch23.XConeDual
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Representable Functors"
    [ testGroup
        "Hx"
        [ testCase "source and destination" $ do
            let (XCone2 _ _ f _ _) = xcone2
                f' = liftH X f

            -- exercise and verify
            assertBool "source" $ (not . null . samples . source) f'
            assertBool "destination" $ (not . null . samples . dest) f',
          testCase "identity" $ do
            let (XCone2 _ _ f g _) = xcone2
                f' = liftH X f
                g' = liftH X g
                idA = identity $ source f'

            -- exercise and verify
            f' <.> idA @?= f'
            idA <.> g' @?= g',
          --
          testCase "composition" $ do
            let (XCone2 _ _ f _ h) = xcone2
                f' = liftH X f
                h' = liftH X h

            -- exercise and verify
            h' <.> f' @?= liftH X (h C.<.> f)
        ],
      testGroup
        "Hx dual"
        [ testCase "source" $ do
            let (XConeDual2 _ _ f _ _) = xconeDual2
                f' = liftH' X' f

            -- exercise and verify
            assertBool "source" $ (not . null . samples . source) f'
            assertBool "destination" $ (not . null . samples . dest) f',
          --
          testCase "identity" $ do
            let (XConeDual2 _ _ f g _) = xconeDual2
                f' = liftH' X' f
                g' = liftH' X' g
                idA = identity $ source f'

            -- exercise and verify
            f' <.> idA @?= f'
            idA <.> g' @?= g',
            ---
            testCase "composition" $ do
             let (XConeDual2 _ _ f _ h) = xconeDual2
                 f' = liftH' X' f
                 h' = liftH' X' h

             -- exercise and verify
             f' <.> h' @?= liftH' X' (h C.<.> f)
        ]
    ]
