module Ch12.ChapterTest (chapterTests) where

import Lib.SetCategoryTest
import Test.Tasty

chapterTests :: TestTree
chapterTests = testGroup "Chapter 12" [Lib.SetCategoryTest.unitTests]
