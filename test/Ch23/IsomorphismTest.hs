module Ch23.IsomorphismTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.RepresentableFunctor
import Ch23.XCone
import Test.Tasty
import Test.Tasty.HUnit

find :: XConeObject -> XConeObject -> C.Morphism XConeObject SumLabel
find = C.findIn C.morphisms

findOp :: XConeObject -> XConeObject -> C.Morphism XConeObject SumLabel
findOp = C.findIn C.opposite

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
        let f = findOp B A
            g = findOp A B
            s = findOp A X
            t = findOp B X

        f C.<.> g @?= C.sumIdentity A
        g C.<.> f @?= C.sumIdentity B
        t C.<.> g @?= s
        s C.<.> f @?= t,
      --
      testCase "hom set dual" $ do
        let f = findOp A B
            g = findOp B A

        let f' = liftH' X f
            g' = liftH' X g

        -- exercise and verify
        f' <.> g' @?= (identity $ source g')
        g' <.> f' @?= (identity $ source f')
    ]
