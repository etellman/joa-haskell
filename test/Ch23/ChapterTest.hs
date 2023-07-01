module Ch23.ChapterTest (chapterTests) where

import Ch23.IsomorphismTest
import Ch23.RepresentableFunctorTest
import Ch23.YonedaTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 23"
    [ Ch23.IsomorphismTest.tests,
      Ch23.RepresentableFunctorTest.tests,
      Ch23.YonedaTest.tests
    ]
