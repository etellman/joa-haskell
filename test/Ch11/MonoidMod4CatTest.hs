module Ch11.MonoidMod4CatTest (unitTests) where

import Lib.Category
import Lib.MonoidCat
import Ch11.MonoidMod4Cat
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

pmlabel :: Morphism MonoidObject Mod4Label -> Int
pmlabel = m4label . mlabel

identity :: Morphism MonoidObject Mod4Label
identity = head $ filter ((== 0) . pmlabel) morphisms

genMonoidMorphism :: Gen (Morphism MonoidObject Mod4Label)
genMonoidMorphism = Gen.element morphisms

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
      actual === (mod4Morphism $ (pmlabel f + pmlabel g) `rem` 4)

prop_group :: Property
prop_group =
  property $
    do
      -- set up
      f <- forAll $ genMonoidMorphism

      -- exercise
      let negf = mod4Morphism $ (4 - (pmlabel f)) `rem` 4

      -- verify
      assert $ negf `elem` morphisms
      f <.> negf === identity
      negf <.> f === identity

unitTests :: TestTree
unitTests =
  testGroup
    "Monoid Mod 4 Addition Category"
    [ testProperty "id . f == f . id == f" prop_identity,
      testProperty "compose" prop_compose,
      testProperty "every morphism has a negative" prop_group
    ]
