module Ch15.ChapterTest (chapterTests) where

import Ch15.IntSetMonicEpicTest
import Ch15.MonicTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 15"
    [ Ch15.MonicTest.unitTests,
      Ch15.IntSetMonicEpicTest.unitTests
    ]
