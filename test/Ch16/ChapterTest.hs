module Ch16.ChapterTest (chapterTests) where

import Assertions.HUnit
import Lib.SetCategory
import Data.Void
import Test.Tasty
import Test.Tasty.HUnit

idInt :: SetMorphism Int Int
idInt = identity integers

idSingleton :: SetMorphism () ()
idSingleton = identity singleton

idEmpty :: SetMorphism Void Void
idEmpty = identity emptySet

intToSingleton :: SetMorphism Int ()
intToSingleton = SetMorphism integers "toSingleton" (\_ -> ()) singleton

emptyToInt :: SetMorphism Void Int
emptyToInt = SetMorphism emptySet "emptyToEnt" absurd integers

emptyToInt2 :: SetMorphism Void Int
emptyToInt2 = SetMorphism emptySet "emptyToInt2" absurd (multiplesOf 2)

int2ToInt :: SetMorphism Int Int
int2ToInt = SetMorphism (multiplesOf 2) "int2ToInt" id integers

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 16"
    [ testCase "uniqueness - p. 212" $ do
        -- set up
        let intsToTwos = SetMorphism integers "(* 2)" (* 2) (multiplesOf 2)
            twosToInts = SetMorphism (multiplesOf 2) "(/ 2)" (flip div 2) integers

        -- exercise
        let id' = twosToInts <.> intsToTwos

        -- verify
        assertValid id'
        id' @=? SetMorphism integers "id" id integers,
      --
      testGroup
        "Set Category"
        [ testGroup
            "initial"
            [ testCase "idInt . emptyToInt" $ identity integers <.> emptyToInt @?= emptyToInt,
              testCase "emptyToInt . idEmpty" $ do emptyToInt <.> idEmpty @?= emptyToInt,
              testCase "idEmpty . idEmpty" $ idEmpty <.> idEmpty @?= idEmpty,
              --
              -- f == g iff f(x) == g(x) for all x, but for the empty set "for all x"
              -- is always true, so it doesn't matter what the function is
              testCase "unique" $ do
                -- set up
                let f = SetMorphism emptySet "emptyToEnt" (\_ -> 17) integers
                    f' = SetMorphism emptySet "emptyToEnt" (\_ -> 31) integers
                    f'' = int2ToInt <.> emptyToInt2

                -- exercise and verify
                f @?= emptyToInt
                f' @?= emptyToInt
                f'' @?= emptyToInt,
              --
              testCase "not equal" $ do
                assertBool "from initial to different sets" $ emptyToInt /= emptyToInt2
            ],
          testGroup
            "terminal"
            [ testCase "intToSingleton . idInt" $ do
                intToSingleton <.> idInt @?= intToSingleton,
              --
              testCase "idSingleton . intToSingleton" $ do
                idSingleton <.> intToSingleton @?= intToSingleton,
              --
              testCase "idSingleton . idSingleton" $ idSingleton <.> idSingleton @?= idSingleton
            ]
        ]
    ]
