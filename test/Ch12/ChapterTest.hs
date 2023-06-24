module Ch12.ChapterTest (chapterTests) where

import Ch12.SetCategoryTest
import Test.Tasty

chapterTests :: TestTree
chapterTests = testGroup "Chapter 12" [Ch12.SetCategoryTest.unitTests]
