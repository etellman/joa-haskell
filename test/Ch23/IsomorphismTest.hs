module Ch23.IsomorphismTest (tests) where

import Ch12.SetCategory
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

data XCone = XCone
  { coneS :: SetMorphism Int Int,
    coneT :: SetMorphism Int Int,
    coneF :: SetMorphism Int Int,
    coneG :: SetMorphism Int Int
  }
  deriving (Show)

xcone :: Int -> Int -> Int -> XCone
xcone n m k =
  let f = multiply n
      g = divide n
      x = multiplesOf (m * n)
   in XCone
        { coneS = add x (source f) n,
          coneT = add x (source g) (k * n),
          coneF = f,
          coneG = g
        }

genCone :: Gen XCone
genCone =
  do
    n <- Gen.int (Range.constant 1 200)
    m <- Gen.int (Range.constant 1 10)
    k <- Gen.int (Range.constant 1 10)
    return $ xcone n m k

prop_baseIsomorphism :: Property
prop_baseIsomorphism = property $ do
  -- set up
  (XCone s t f g) <- forAll genCone

  -- exercise and verify
  f <.> g === (identity $ source g)
  g <.> f === (identity $ source f)
  g <.> f <.> s === s
  f <.> g <.> t === t

prop_homSet :: Property
prop_homSet = property $ do
  -- set up
  (XCone s t f g) <- forAll genCone

  let cxa = finiteSet "Cxa" [s, g <.> t]
      cxb = finiteSet "Cxb" [t, f <.> s]

      cxaToCxb = SetMorphism cxa "f . _" (f <.>) cxb
      cxbToCxa = SetMorphism cxb "g . _" (g <.>) cxa

  -- exercise and verify
  cxbToCxa <.> cxaToCxb === (identity cxa)
  cxaToCxb <.> cxbToCxa === (identity cxb)

tests :: TestTree
tests =
  testGroup
    "Isomorphism"
    [ testProperty "base isomorphism" prop_baseIsomorphism,
      testProperty "Hom set" prop_homSet
    ]
