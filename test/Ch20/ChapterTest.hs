module Ch20.ChapterTest (chapterTests) where

import Ch20.AbTest
import Ch20.CharacterCodeTest
import Ch20.FunctorLawsTest
import Ch20.PosetFunctorTest
import Ch20.StructureNativeTest
import Ch20.StructureTest
import Ch20.SplitEpicTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 20"
    [ Ch20.PosetFunctorTest.tests,
      Ch20.FunctorLawsTest.tests,
      Ch20.AbTest.tests,
      Ch20.CharacterCodeTest.tests,
      Ch20.StructureNativeTest.tests,
      Ch20.StructureTest.tests,
      Ch20.SplitEpicTest.tests
    ]
