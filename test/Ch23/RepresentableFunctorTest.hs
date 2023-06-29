module Ch23.RepresentableFunctorTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.RepresentableFunctor
import Ch23.XCone
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Representable Functors"
    [ testCase "identity 2" $ do
        let (XCone2 _ _ f g _) = xcone2
            cxaId = liftR X (C.sumIdentity A)
            f' = liftR X f
            g' = liftR X g

        -- exercise and verify
        f' <.> cxaId @?= f'
        cxaId <.> g' @?= g',
      --
      testCase "composition" $ do
        let (XCone2 _ _ f _ h) = xcone2
            f' = liftR X f
            h' = liftR X h

        -- exercise and verify
        h' <.> f' @?= liftR X (h C.<.> f)
    ]
