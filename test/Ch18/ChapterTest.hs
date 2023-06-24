module Ch18.ChapterTest (chapterTests) where

import Ch18.Abc123Test
import Ch18.LcmProductTest
import Ch18.ProductTest
import Ch18.SetCoproductTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 18"
    [ Ch18.Abc123Test.tests,
      Ch18.ProductTest.tests,
      Ch18.LcmProductTest.tests,
      Ch18.SetCoproductTest.tests
    ]
