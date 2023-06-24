module Ch06.Ch06Test (chapterTests) where

import Ch06.DistanceTest
import Test.Tasty

chapterTests :: TestTree
chapterTests = testGroup "Chapter 6" [ Ch06.DistanceTest.unitTests ]
