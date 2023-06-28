module Ch23.RepresentableFunctorTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.XCone
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Representable Functors"
    [ testCase "identity" $ do
        let (XCone2 s t f g _) = xcone2
            cxa = finiteSet "Cxa" [s, g C.<.> t]
            cxb = finiteSet "Cxb" [t, f C.<.> s]

            cxaId = SetMorphism cxa "id . _" ((C.sumIdentity A) C.<.>) cxa
            cxaToCxb = SetMorphism cxa "f . _" (f C.<.>) cxb
            cxbToCxa = SetMorphism cxb "g . _" (g C.<.>) cxa

        -- exercise and verify
        cxaId @?= (identity cxa)
        cxaToCxb <.> cxaId @?= cxaToCxb
        cxaId <.> cxbToCxa @?= cxbToCxa,
      --
      testCase "composition" $ do
        let (XCone2 s t f g h) = xcone2
            cxa = finiteSet "Cxa" [s, g C.<.> t]
            cxb = finiteSet "Cxb" [t, f C.<.> s]
            cbc = finiteSet "Cbc" [h]

            cxaToCxb = SetMorphism cxa "f . _" (f C.<.>) cxb
            cxbToCbc = SetMorphism cxb "h . _" (h C.<.>) cbc

        -- exercise and verify
        --
        cxbToCbc <.> cxaToCxb @?= SetMorphism cxa "h . f . _" (h C.<.> f C.<.>) cbc
    ]
