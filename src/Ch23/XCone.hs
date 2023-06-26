module Ch23.XCone
  ( XConeObject (..),
    SumLabel (..),
    xcone2,
    XCone2 (..),
  )
where

import Ch08.Category

data XConeObject = A | B | X deriving (Eq, Show)

instance Objects XConeObject where
  objects = [A, B, X]

data XCone2 = XCone2
  { xcS :: Morphism XConeObject SumLabel,
    xcT :: Morphism XConeObject SumLabel,
    xcF :: Morphism XConeObject SumLabel,
    xcG :: Morphism XConeObject SumLabel
  }
  deriving (Show)

xcone2 :: XCone2
xcone2 =
  XCone2
    { xcS = Morphism X (SumLabel 2) A,
      xcT = Morphism X (SumLabel 3) B,
      xcF = Morphism A (SumLabel 1) B,
      xcG = Morphism B (SumLabel (-1)) A
    }

instance Morphisms XConeObject SumLabel where
  morphisms =
    let (XCone2 s t f g) = xcone2
     in [s, t, f, g]
