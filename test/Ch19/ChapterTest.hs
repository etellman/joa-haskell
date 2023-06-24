module Ch19.ChapterTest (chapterTests) where

import Ch19.SetPullbackTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 19"
    [Ch19.SetPullbackTest.tests]
