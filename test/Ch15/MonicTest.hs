module Ch15.MonicTest (unitTests) where

import Ch08.Category
import Ch11.MonoidCat
import Ch11.MonoidPlusCat
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(==>) :: (MonadTest m) => Bool -> Bool -> m ()
a ==> b = H.assert $ not a || b

genPlusMorphism :: (Range Int) -> Gen (Morphism MonoidObject PlusLabel)
genPlusMorphism r = fmap plusMorphism $ Gen.int r

prop_injectiveFunction :: Property
prop_injectiveFunction =
  property $ do
    -- set up
    let anyInt = forAll $ Gen.int (Range.constant 0 7)
    x <- anyInt
    y <- anyInt
    z <- anyInt

    -- exercise and verify
    (x + z == y + z) ==> (x == y)

prop_monicMorphism :: Property
prop_monicMorphism =
  property $ do
    -- set up
    let anyMorphism = forAll $ genPlusMorphism (Range.constant 0 7)
    s <- anyMorphism
    t <- anyMorphism
    f <- anyMorphism

    -- exercise and verify
    (f <.> s == f <.> t) ==> (s == t)

prop_epicMorphism :: Property
prop_epicMorphism =
  property $ do
    -- set up
    let anyMorphism = forAll $ genPlusMorphism (Range.constant 0 7)
    s <- anyMorphism
    t <- anyMorphism
    f <- anyMorphism

    -- exercise and verify
    (s <.> f == t <.> f) ==> (s == t)

unitTests :: TestTree
unitTests =
  testGroup
    "Injective Functions, Monics and Epics -- p. 186 - 199"
    [ testGroup
        "Monic"
        [ testProperty "injective function" prop_injectiveFunction,
          testProperty "morphism" prop_monicMorphism
        ],
      testProperty "epic morphism" prop_epicMorphism
    ]
