module Ch08.Factor30Test (unitTests) where

import Lib.Category
import Ch08.Factor30
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

m30label :: Morphism Factor30 Factor30Label -> Int
m30label = f30label . mlabel

-- | identity . f
prop_identity :: Property
prop_identity =
  property $ do
    f <- forAll $ Gen.element (morphisms :: [Morphism Factor30 Factor30Label])
    let afterF = morphismsFrom (dest f) :: [Morphism Factor30 Factor30Label]
    let identity = head $ filter ((== 1) . m30label) afterF

    -- exercise and verify
    identity <.> f === f

-- | compose two compatible morphisms
prop_compose :: Property
prop_compose =
  property $ do
    g <- forAll $ Gen.element (morphisms :: [Morphism Factor30 Factor30Label])
    f <- forAll $ Gen.element (morphismsFrom (dest g))

    -- exercise
    let actual = f <.> g

    -- verify
    actual === Morphism (source g) (Factor30Label $ m30label f * m30label g) (dest f)

unitTests :: TestTree
unitTests =
  testGroup
    "Factor30"
    [ testCase "morphisms from 5" $
        do
          -- exercise
          let ms = morphismsFrom (Factor30 10)

          -- verify
          ms
            @?= [ Morphism
                    { source = Factor30 10,
                      mlabel = Factor30Label 10,
                      dest = Factor30 1
                    },
                  Morphism
                    { source = Factor30 10,
                      mlabel = Factor30Label 5,
                      dest = Factor30 2
                    },
                  Morphism
                    { source = Factor30 10,
                      mlabel = Factor30Label 2,
                      dest = Factor30 5
                    },
                  Morphism
                    { source = Factor30 10,
                      mlabel = Factor30Label 1,
                      dest = Factor30 10
                    }
                ],
      --
      testCase "all morphisms" $
        do
          -- exercise
          let ms = morphisms :: [Morphism Factor30 Factor30Label]

          -- verify
          length ms @?= 27,
      --
      testProperty "identity" prop_identity,
      testProperty "compose" prop_compose
    ]
