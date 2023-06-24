module Ch08.Factor30
  ( Factor30 (..),
    Factor30Label (..),
  )
where

import Ch08.Category

-- | All the divisors of a number
divisors :: Int -> [Int]
divisors x = filter ((== 0) . mod x) [1 .. x]

-- | New type, so it can be an instance of Category
newtype Factor30 = Factor30 {f30factor :: Int} deriving (Eq, Ord, Show)

newtype Factor30Label = Factor30Label {f30label :: Int} deriving (Eq, Ord, Show)

instance Semigroup Factor30Label where
  (Factor30Label l1) <> (Factor30Label l2) = Factor30Label (l1 * l2)

instance Objects Factor30 where
  objects = fmap Factor30 (divisors 30)

instance Morphisms Factor30 Factor30Label where
  morphismsFrom x =
    let isMultiple y = f30factor x `mod` f30factor y == 0
        toLabel y = Factor30Label (f30factor x `div` f30factor y)
        toMorphism y = Morphism x (toLabel y) y
     in fmap toMorphism $ filter isMultiple objects
