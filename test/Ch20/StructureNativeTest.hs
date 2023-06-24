module Ch20.StructureNativeTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- p. 303 using the Haskell Functor type class

data Singleton a = Singleton () deriving (Show, Eq)

instance Functor Singleton where
  fmap _ (Singleton ()) = Singleton ()

prop_isomorphism :: Property
prop_isomorphism = property $ do
  -- set up
  word <- forAll $ Gen.string (Range.constant 5 10) Gen.lower
  let f = (++ "a")

  -- exercise
  let f' = fmap f

  H.assert $ f word /= word
  f' (Singleton ()) === Singleton ()
  (f' . f') (Singleton ()) === Singleton ()

prop_commute :: Property
prop_commute = property $ do
  -- set up
  word <- forAll $ Gen.string (Range.constant 5 10) Gen.lower
  let f = (++ "a")
      g = (++ "b")

  -- exercise
  let f' = fmap f
      g' = fmap g

  -- verify
  H.assert $ f word /= g word
  f' (Singleton ()) === g' (Singleton ())

tests :: TestTree
tests =
  testGroup
    "Reflection with native Haskell - p. 303"
    [ testProperty "f not isomorphism, Ff isomorphism" prop_isomorphism,
      testProperty "f and g don't commute, f' and g' do commute" prop_commute
    ]
