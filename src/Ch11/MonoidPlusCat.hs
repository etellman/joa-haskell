module Ch11.MonoidPlusCat
  (
    PlusLabel (..),
    plusMorphism,
  )
where

import Lib.Category
import Ch11.MonoidCat

newtype PlusLabel = PlusLabel {plabel :: Int} deriving (Eq, Ord, Show)

instance Semigroup PlusLabel where
  (PlusLabel l1) <> (PlusLabel l2) = PlusLabel (l1 + l2)

instance Morphisms MonoidObject PlusLabel where
  morphisms =
    let nonZero = concat $ fmap (\x -> [x, -x]) [1 ..]
     in fmap plusMorphism (0 : nonZero)

plusMorphism :: Int -> Morphism MonoidObject PlusLabel
plusMorphism l = Morphism MonoidObject (PlusLabel l) MonoidObject
