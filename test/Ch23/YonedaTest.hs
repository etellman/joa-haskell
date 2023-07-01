module Ch23.YonedaTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.RepresentableFunctor
import Ch23.XCone
import Ch23.Yoneda
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Yoneda Lemma"
    [ testCase "natural transformation" $ do
        let f = find A B
            f' = liftH X f
            h = find B C
            h' = liftH X h

        -- exercise and verify
        h' <.> f' @?= liftH X (h C.<.> f)
    ]
