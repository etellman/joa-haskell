module Ch11.ChapterTest (chapterTests) where

import Ch11.MonoidPlusCatTest
import Ch11.MonoidPlusSetTest
import Ch11.MonoidMod4SetTest
import Ch11.MonoidMod4CatTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 11"
    [ Ch11.MonoidPlusCatTest.unitTests,
      Ch11.MonoidPlusSetTest.unitTests,
      Ch11.MonoidMod4SetTest.unitTests,
      Ch11.MonoidMod4CatTest.unitTests
    ]
