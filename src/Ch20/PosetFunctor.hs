module Ch20.PosetFunctor
  ( PosetLabel (..),
    PosetInt (..),
    PosetInt2 (..),
    posetLabel,
    posetValues,
    fObject,
    fMorphism,
  )
where

import Ch08.Category

-- pp. 293 - 294

newtype PosetLabel = PosetLabel {pslabel :: String} deriving (Eq, Ord, Show)

posetLabel :: PosetLabel
posetLabel = PosetLabel "<="

posetValues :: [Int]
posetValues = [0 .. 50]

instance Semigroup PosetLabel where
  (PosetLabel _) <> (PosetLabel _) = posetLabel

newtype PosetInt = PosetInt {psvalue :: Int} deriving (Eq, Ord, Show)

instance Objects PosetInt where
  objects = fmap PosetInt posetValues

instance Morphisms PosetInt PosetLabel where
  morphismsFrom (PosetInt x) = do
    y <- [x .. maximum posetValues]
    return (Morphism (PosetInt x) posetLabel (PosetInt y))

newtype PosetInt2 = PosetInt2 {psvalue2 :: Int} deriving (Eq, Ord, Show)

fObject :: PosetInt -> PosetInt2
fObject (PosetInt x) = PosetInt2 (2 * x)

fMorphism :: Morphism PosetInt PosetLabel -> Morphism PosetInt2 PosetLabel
fMorphism (Morphism x l y) = Morphism (fObject x) l (fObject y)

instance Objects PosetInt2 where
  objects = fmap fObject objects

instance Morphisms PosetInt2 PosetLabel where
  morphisms = fmap fMorphism (morphisms :: [Morphism PosetInt PosetLabel])
