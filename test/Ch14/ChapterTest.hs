module Ch14.ChapterTest (chapterTests) where

import Ch14.AbcCategoryTest
import Ch14.IntSetsTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 14"
    [ Ch14.AbcCategoryTest.unitTests,
      Ch14.IntSetsTest.unitTests
    ]
