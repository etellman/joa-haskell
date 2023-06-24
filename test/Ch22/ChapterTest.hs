module Ch22.ChapterTest (chapterTests) where

import Ch22.NaturalTransformationTest
import Ch22.SetFunctorCategoryTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 22"
    [ Ch22.NaturalTransformationTest.tests,
      Ch22.SetFunctorCategoryTest.tests
    ]
