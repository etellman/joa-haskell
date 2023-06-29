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
            f' = liftR X f
            g' = liftR X g
            idA = identity $ source f'

        -- exercise and verify
        f' <.> idA @?= f'
        idA <.> g' @?= g',
      --
      testCase "composition" $ do
        let (XCone2 _ _ f _ h) = xcone2
            f' = liftR X f
            h' = liftR X h

        -- exercise and verify
        h' <.> f' @?= liftR X (h C.<.> f)
    ]
