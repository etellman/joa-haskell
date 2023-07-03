module Ch11.MonoidPlusCatTest (unitTests) where

import Lib.Category
import Ch11.MonoidCat
import Ch11.MonoidPlusCat
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

pmlabel :: Morphism MonoidObject PlusLabel -> Int
pmlabel = plabel . mlabel

monoidMorphisms :: [Morphism MonoidObject PlusLabel]
monoidMorphisms = take 1000 morphisms

identity :: Morphism MonoidObject PlusLabel
identity = head $ filter ((== 0) . pmlabel) monoidMorphisms

genMonoidMorphism :: Gen (Morphism MonoidObject PlusLabel)
genMonoidMorphism = Gen.element monoidMorphisms

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    f <- forAll $ genMonoidMorphism

    -- exercise and verify
    identity <.> f === f
    f <.> identity === f

prop_compose :: Property
prop_compose =
  property $
    do
      -- set up
      f <- forAll $ genMonoidMorphism
      g <- forAll $ genMonoidMorphism

      -- exercise
      let actual = f <.> g

      -- verify
      actual === (plusMorphism $ pmlabel f + pmlabel g)

prop_group :: Property
prop_group =
  property $
    do
      -- set up
      f <- forAll $ genMonoidMorphism

      -- exercise
      let negf = plusMorphism $ (negate . pmlabel) f

      -- verify
      f <.> negf === identity
      negf <.> f === identity

unitTests :: TestTree
unitTests =
  testGroup
    "Monoid Addition Category"
    [ testProperty "id . f == f . id == f" prop_identity,
      testProperty "compose" prop_compose,
      testProperty "every morphism has a negative" prop_group
    ]
