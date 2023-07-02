module Ch23.YonedaTestCategory
  ( find,
    findOp,
    YonedaObject (..),
  )
where

import Ch08.Category

data YonedaObject = A | B | X | Y deriving (Eq, Show)

instance Objects YonedaObject where
  objects = [A, B, X, Y]

instance Morphisms YonedaObject SumLabel where
  morphisms =
    [ Morphism A (SumLabel 1) B,
      Morphism X (SumLabel 1) Y,
      Morphism X (SumLabel 1) A,
      Morphism X (SumLabel 1) B,
      Morphism Y (SumLabel 2) A,
      Morphism Y (SumLabel 2) B
    ]

find :: YonedaObject -> YonedaObject -> Morphism YonedaObject SumLabel
find = findIn morphisms

findOp :: YonedaObject -> YonedaObject -> Morphism YonedaObject SumLabel
findOp = findIn opposite
