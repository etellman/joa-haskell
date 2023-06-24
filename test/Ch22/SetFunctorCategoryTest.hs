module Ch22.SetFunctorCategoryTest (tests) where

import Ch12.SetCategory
import Ch20.SetFunctor
import Functors.FFunctor
import Functors.GFunctor
import Functors.HFunctor
import Functors.KFunctor
import Functors.PFunctor
import Functors.QFunctor
import Functors.RFunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

prop_natural :: Property
prop_natural = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 1000)

  let f = multiply n
      ff = lift f :: SetMorphism (F Int) (F Int)
      gf = lift f :: SetMorphism (G Int) (G Int)
      ax = natural $ source ff
      ay = natural $ dest ff

  -- exercise and verify
  gf <.> ax === ay <.> ff

prop_isomorphism :: Property
prop_isomorphism = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 1000)

  let f = multiply n
      ff = lift f :: SetMorphism (F Int) (F Int)
      gf = lift f :: SetMorphism (G Int) (G Int)

      ax = natural $ source ff
      bx = natural $ source gf

      ay = natural $ dest ff
      by = natural $ dest gf

  -- exercise and verify
  ff <.> bx <.> ax === ff
  by <.> ay <.> ff === ff

prop_horizontalComposition1 :: Property
prop_horizontalComposition1 = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 1000)

  let f = multiply n
      hff = lift . lift $ f :: SetMorphism (H (F Int)) (H (F Int))
      kgf = lift . lift $ f :: SetMorphism (K (G Int)) (K (G Int))

      ax = lift $ natural (source $ lift f) :: SetMorphism (H (F Int)) (H (G Int))
      ay = lift $ natural (dest $ lift f)

      bx = natural $ source (lift . lift $ f) :: SetMorphism (H (G Int)) (K (G Int))
      by = natural $ dest (lift . lift $ f)

  kgf <.> bx <.> ax === by <.> ay <.> hff

prop_horizontalComposition2 :: Property
prop_horizontalComposition2 = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 2 1000)

  let f = multiply n
      hff = lift . lift $ f :: SetMorphism (H (F Int)) (H (F Int))
      kgf = lift . lift $ f :: SetMorphism (K (G Int)) (K (G Int))

      ax = lift $ natural (source $ lift f) :: SetMorphism (K (F Int)) (K (G Int))
      ay = lift $ natural (dest $ lift f)

      bx = natural $ source (lift . lift $ f) :: SetMorphism (H (F Int)) (K (F Int))
      by = natural $ dest (lift . lift $ f)

  kgf <.> ax <.> bx === ay <.> by <.> hff

tests :: TestTree
tests =
  testGroup
    "Set Functor Category"
    [ testProperty "natural" prop_natural,
      testProperty "isomorphism" prop_isomorphism,
      testProperty "horizontal composition 1" prop_horizontalComposition1,
      testProperty "horizontal composition 2" prop_horizontalComposition2,
      testCase "interchange" $ do
        -- set up
        let alpha = natural (intFs) :: SetMorphism (F Int) (G Int)
            beta = natural (dest alpha) :: SetMorphism (G Int) (H Int)

            pAlpha = lift alpha :: SetMorphism (P (F Int)) (P (G Int))
            deltaH = natural $ dest qBeta :: SetMorphism (Q (H Int)) (R (H Int))

            gammaG = natural $ dest pAlpha :: SetMorphism (P (G Int)) (Q (G Int))
            gammaH = natural $ dest pBeta :: SetMorphism (P (H Int)) (Q (H Int))

            pBeta = lift beta :: SetMorphism (P (G Int)) (P (H Int))
            qBeta = lift beta :: SetMorphism (Q (G Int)) (Q (H Int))

        deltaH <.> gammaH <.> pBeta <.> pAlpha @?= deltaH <.> qBeta <.> gammaG <.> pAlpha
    ]
