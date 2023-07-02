module Ch23.YonedaTest (tests) where

import qualified Ch08.Category as C
import Ch12.SetCategory
import Ch23.RepresentableFunctor
import Ch23.Yoneda
import Ch23.YonedaTestCategory
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Yoneda Lemma"
    [ testCase "f commuting" $ do
        -- set up
        let f = find X Y
            ay = findOp A Y
            ax = findOp A X

        -- exercise and verify
        f C.<.> ax @?= ay,
      --
      testCase "natural transformation" $ do
        -- set up
        let f = find X Y

        -- exercise
        let hfa = naturalH f (homSet' A X)

        -- verify
        source hfa @?= homSet' A X
        dest hfa @?= homSet' A Y,
      --
      testCase "Yoneda square" $ do
        let f = find X Y
            p = find A B
            homXp = liftH' X p
            natFa = naturalH f (homSet' A X)

            homYp = liftH' Y p
            natFb = naturalH f (homSet' B X)

        natFa <.> homXp @?= homYp <.> natFb
    ]
