module Ch20.PosetFunctorTest (tests) where

import Assertions.Hedgehog
import Ch08.Category
import Ch20.PosetFunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

-- | The expected morphism in PosetInt2 if there is a morphism from x to y in PosetInt
int2Morphism :: Int -> Int -> Morphism PosetInt2 PosetLabel
int2Morphism x y = Morphism (PosetInt2 (2 * x)) posetLabel (PosetInt2 (2 * y))

prop_objects :: Property
prop_objects = property $ do
  -- set up
  (PosetInt x) <- forAll $ Gen.element (objects :: [PosetInt])

  -- exercise and verify
  assert $ (PosetInt2 (2 * x)) `elem` objects

prop_morphisms :: Property
prop_morphisms = property $ do
  -- set up
  x <- forAll $ Gen.element posetValues
  y <- forAll $ Gen.element posetValues
  H.cover 30 "<=" $ x <= y
  H.cover 30 ">" $ x > y

  -- exercise and verify
  (x <= y) <==> int2Morphism x y `elem` morphisms

prop_equalMorphism :: Property
prop_equalMorphism = property $ do
  -- set up
  x <- forAll $ Gen.element posetValues

  -- exercise and verify
  assert $ int2Morphism x x `elem` morphisms

prop_compose :: Property
prop_compose = property $ do
  -- set up
  g@(Morphism gsource _ gdest) <- forAll $ Gen.element morphisms
  f@(Morphism _ _ fdest) <- forAll $ Gen.element (morphismsFrom gdest)

  -- exercise
  let fg' = fMorphism f <.> fMorphism g

  -- verify
  fg' === Morphism (fObject gsource) posetLabel (fObject fdest)
  assert $ fg' `elem` morphisms

prop_morphismLaw :: Property
prop_morphismLaw = property $ do
  -- set up
  f@(Morphism s _ d) <- forAll $ Gen.element morphisms

  -- exercise
  let f' = fMorphism f

  -- verify
  f' === Morphism (fObject s) posetLabel (fObject d)
  assert $ f' `elem` morphisms

prop_identity :: Property
prop_identity = property $ do
  -- set up
  id1@(Morphism s _ _) <- forAll $ Gen.element (filter isId morphisms)

  -- exercise
  let id2 = fMorphism id1

  -- verify
  id2 === Morphism (fObject s) posetLabel (fObject s)
  assert $ id2 `elem` morphisms

tests :: TestTree
tests =
  testGroup
    "Poset Functor"
    [ testProperty "objects" prop_objects,
      testProperty "morphisms" prop_morphisms,
      testProperty "equal morphisms" prop_equalMorphism,
      testProperty "compose" prop_compose,
      testProperty "morphism law" prop_morphismLaw,
      testProperty "identity" prop_identity
    ]
