module Ch23.XCone
  ( find,
    findOpposite,
    XConeObject (..),
  )
where

import Ch08.Category

data XConeObject = A | B | C | X deriving (Eq, Show)

instance Objects XConeObject where
  objects = [A, B, C, X]

instance Morphisms XConeObject SumLabel where
  morphisms =
    [ Morphism X (SumLabel 2) A,
      Morphism X (SumLabel 3) B,
      Morphism A (SumLabel 1) B,
      Morphism B (SumLabel (-1)) A,
      Morphism B (SumLabel 10) C
    ]

find :: XConeObject -> XConeObject -> Morphism XConeObject SumLabel
find = findIn morphisms

findOpposite :: XConeObject -> XConeObject -> Morphism XConeObject SumLabel
findOpposite = findIn opposite
