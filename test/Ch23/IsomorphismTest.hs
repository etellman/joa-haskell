module Ch23.IsomorphismTest (tests) where

import Assertions.Hedgehog
import Ch12.SetCategory
import qualified Ch08.Category as C
import Ch23.XCone
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

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

dualCone :: Int -> Int -> Int -> XCone
dualCone n m k =
  let f = multiply n
      g = divide n
      x = multiplesOf (m * n)
   in XCone
        { coneS = add (source f) x n,
          coneT = add (source g) x (k * n),
          coneF = f,
          coneG = g
        }

genCone :: (Int -> Int -> Int -> XCone) -> Gen XCone
genCone f =
  do
    n <- Gen.int (Range.constant 1 100)
    m <- Gen.int (Range.constant 1 100)
    k <- Gen.int (Range.constant 1 100)

    return $ f n m k

prop_baseIsomorphism :: Property
prop_baseIsomorphism = property $ do
  -- set up
  (XCone s t f g) <- forAll $ genCone xcone

  -- exercise and verify
  f <.> g === (identity $ source g)
  g <.> f === (identity $ source f)
  g <.> f <.> s === s
  f <.> g <.> t === t

prop_homSet :: Property
prop_homSet = property $ do
  -- set up
  (XCone s t f g) <- forAll $ genCone xcone

  let cxa = finiteSet "Cxa" [s, g <.> t] :: SetObject (SetMorphism Int Int)
      cxb = finiteSet "Cxb" [t, f <.> s]

  cxaToCxb <- assertValid $ SetMorphism cxa "f . _" (f <.>) cxb
  cxbToCxa <- assertValid $ SetMorphism cxb "g . _" (g <.>) cxa

  -- exercise and verify
  cxbToCxa <.> cxaToCxb === (identity cxa)
  cxaToCxb <.> cxbToCxa === (identity cxb)

prop_dual :: Property
prop_dual = property $ do
  -- set up
  (XCone s t f g) <- forAll $ genCone dualCone

  let cax = finiteSet "Cxa" [s, t <.> f] :: SetObject (SetMorphism Int Int)
      cbx = finiteSet "Cxb" [t, s <.> g]

  caxToCbx <- assertValid $ SetMorphism cax "_ . g" (<.> g) cbx
  cbxToCax <- assertValid $ SetMorphism cbx "_ . f" (<.> f) cax

  -- exercise and verify
  cbxToCax <.> caxToCbx === (identity cax)
  caxToCbx <.> cbxToCax === (identity cbx)

tests :: TestTree
tests =
  testGroup
    "Isomorphism"
    [ testProperty "base isomorphism" prop_baseIsomorphism,
      testProperty "hom set" prop_homSet,
      testProperty "dual category" prop_dual,
      testCase "base isomorphism 2" $ do
        let (XCone2 s t f g) = xcone2

        f C.<.> g @?= C.sumIdentity B
        g C.<.> f @?= C.sumIdentity A
        g C.<.> f C.<.> s @?= s
        f C.<.> g C.<.> t @?= t,
      testCase "hom set 2" $ do
        let (XCone2 s t f g) = xcone2

        let cxa = finiteSet "Cxa" [s, g C.<.> t] :: SetObject (C.Morphism XConeObject SumLabel)
            cxb = finiteSet "Cxb" [t, f C.<.> s]

            cxaToCxb = SetMorphism cxa "f . _" (f C.<.>) cxb
            cxbToCxa = SetMorphism cxb "g . _" (g C.<.>) cxa

        -- exercise and verify
        cxbToCxa <.> cxaToCxb @?= (identity cxa)
        cxaToCxb <.> cxbToCxa @?= (identity cxb)
    ]
