module Ch23.IsomorphismTest (tests) where

import qualified Lib.Category as C
import Ch12.SetCategory
import Ch23.RepresentableFunctor
import Ch23.XCone
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Isomorphism"
    [ testCase "base isomorphism 2" $ do
        let f = find A B
            g = find B A
            s = find X A
            t = find X B

        f C.<.> g @?= C.sumIdentity B
        g C.<.> f @?= C.sumIdentity A
        g C.<.> f C.<.> s @?= s
        f C.<.> g C.<.> t @?= t,
      --
      testCase "hom set" $ do
        let f = find A B
            g = find B A

        let f' = liftH X f
            g' = liftH X g

        -- exercise and verify
        f' <.> g' @?= (identity $ source g')
        g' <.> f' @?= (identity $ source f'),
      --
      testCase "dual isomorphism" $ do
        let f = findOpposite B A
            g = findOpposite A B
            s = findOpposite A X
            t = findOpposite B X

        f C.<.> g @?= C.sumIdentity A
        g C.<.> f @?= C.sumIdentity B
        t C.<.> g @?= s
        s C.<.> f @?= t,
      --
      testCase "hom set dual" $ do
        let f = findOpposite A B
            g = findOpposite B A

        let f' = liftH' X f
            g' = liftH' X g

        -- exercise and verify
        f' <.> g' @?= (identity $ source g')
        g' <.> f' @?= (identity $ source f')
    ]
