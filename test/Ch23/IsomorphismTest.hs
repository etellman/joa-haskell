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

        let f' = liftR X f
            g' = liftR X g

        -- exercise and verify
        f' <.> g' @?= (identity $ source f')
        g' <.> f' @?= (identity $ source g'),
      --
      testCase "dual isomorphism" $ do
        let (XConeDual2 s t f g) = xconeDual2

        f C.<.> g @?= C.sumIdentity B'
        g C.<.> f @?= C.sumIdentity A'
        t C.<.> f @?= s
        s C.<.> g @?= t,
      --
      testCase "hom set dual" $ do
        let (XConeDual2 s t f g) = xconeDual2

        let cax = finiteSet "Cxa" [s, t C.<.> f]
            cbx = finiteSet "Cxb" [t, s C.<.> g]

            caxToCbx = SetMorphism cax "_ . g" (C.<.> g) cbx
            cbxToCax = SetMorphism cbx "_ . f" (C.<.> f) cax

        -- exercise and verify
        cbxToCax <.> caxToCbx @?= (identity cax)
        caxToCbx <.> cbxToCax @?= (identity cbx)
    ]
