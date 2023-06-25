import Ch05Test
import Ch06.Ch06Test
import Ch08.Ch08Test
import Ch10Test
import Ch11.ChapterTest
import Ch12.ChapterTest
import Ch14.ChapterTest
import Ch15.ChapterTest
import Ch16.ChapterTest
import Ch18.ChapterTest
import Ch19.ChapterTest
import Ch20.ChapterTest
import Ch22.ChapterTest
import Ch23.ChapterTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "The Joy of Abstraction"
        [ Ch05Test.unitTests,
          Ch06.Ch06Test.chapterTests,
          Ch08.Ch08Test.chapterTests,
          Ch10Test.unitTests,
          Ch11.ChapterTest.chapterTests,
          Ch12.ChapterTest.chapterTests,
          Ch14.ChapterTest.chapterTests,
          Ch15.ChapterTest.chapterTests,
          Ch16.ChapterTest.chapterTests,
          Ch18.ChapterTest.chapterTests,
          Ch19.ChapterTest.chapterTests,
          Ch20.ChapterTest.chapterTests,
          Ch22.ChapterTest.chapterTests,
          Ch23.ChapterTest.chapterTests
        ]
    )
