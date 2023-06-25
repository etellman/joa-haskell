module Ch23.ChapterTest (chapterTests) where

import Ch23.IsomorphismTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 23"
    [ Ch23.IsomorphismTest.tests
    ]
