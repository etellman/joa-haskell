module Ch23.IsomorphismTest (tests) where

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
    "Isomorphism"
    [ testCase "base isomorphism 2" $ do
        let (XCone2 s t f g _) = xcone2

        f C.<.> g @?= C.sumIdentity B
        g C.<.> f @?= C.sumIdentity A
        g C.<.> f C.<.> s @?= s
        f C.<.> g C.<.> t @?= t,
      --
      testCase "hom set" $ do
        let (XCone2 _ _ f g _) = xcone2

        let f' = liftH X f
            g' = liftH X g

        -- exercise and verify
        f' <.> g' @?= (identity $ source g')
        g' <.> f' @?= (identity $ source f'),
      --
      testCase "dual isomorphism" $ do
        let (XConeDual2 s t f g _) = xconeDual2

        f C.<.> g @?= C.sumIdentity B'
        g C.<.> f @?= C.sumIdentity A'
        t C.<.> f @?= s
        s C.<.> g @?= t,
      --
      testCase "hom set dual" $ do
        let (XConeDual2 _ _ f g _) = xconeDual2

        let f' = liftH' X' f
            g' = liftH' X' g

        -- exercise and verify
        f' <.> g' @?= (identity $ source g')
        g' <.> f' @?= (identity $ source f')
    ]
