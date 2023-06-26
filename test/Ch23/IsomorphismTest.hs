module Ch23.IsomorphismTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.XCone
import Ch23.XConeDual
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Isomorphism"
    [ testCase "base isomorphism 2" $ do
        let (XCone2 s t f g) = xcone2

        f C.<.> g @?= C.sumIdentity B
        g C.<.> f @?= C.sumIdentity A
        g C.<.> f C.<.> s @?= s
        f C.<.> g C.<.> t @?= t,
      --
      testCase "hom set 2" $ do
        let (XCone2 s t f g) = xcone2

        let cxa = finiteSet "Cxa" [s, g C.<.> t]
            cxb = finiteSet "Cxb" [t, f C.<.> s]

            cxaToCxb = SetMorphism cxa "f . _" (f C.<.>) cxb
            cxbToCxa = SetMorphism cxb "g . _" (g C.<.>) cxa

        -- exercise and verify
        cxbToCxa <.> cxaToCxb @?= (identity cxa)
        cxaToCxb <.> cxbToCxa @?= (identity cxb),
      --
      testCase "dual isomorphism" $ do
        let (XConeDual2 s t f g) = xconeDual2

        f C.<.> g @?= C.sumIdentity B'
        g C.<.> f @?= C.sumIdentity A'
        t C.<.> f @?= s
        s C.<.> g @?= t,
      --
      testCase "hom set dual 2" $ do
        let (XConeDual2 s t f g) = xconeDual2

        let cax = finiteSet "Cxa" [s, t C.<.> f]
            cbx = finiteSet "Cxb" [t, s C.<.> g]

            caxToCbx = SetMorphism cax "_ . g" (C.<.> g) cbx
            cbxToCax = SetMorphism cbx "_ . f" (C.<.> f) cax

        -- exercise and verify
        cbxToCax <.> caxToCbx @?= (identity cax)
        caxToCbx <.> cbxToCax @?= (identity cbx)
    ]
