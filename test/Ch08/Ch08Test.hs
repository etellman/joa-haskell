module Ch08.Ch08Test (chapterTests) where

import Ch08.Factor30Test
import Test.Tasty

chapterTests :: TestTree
chapterTests = testGroup "Chapter 8" [ Ch08.Factor30Test.unitTests ]
