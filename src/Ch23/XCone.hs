module Ch23.XCone
  ( XConeObject (..),
    SumLabel (..),
    xcone2,
    XCone2 (..),
  )
where

import Ch08.Category

data XConeObject = A | B | C | X deriving (Eq, Show)

instance Objects XConeObject where
  objects = [A, B, C, X]

data XCone2 = XCone2
  { xcS :: Morphism XConeObject SumLabel,
    xcT :: Morphism XConeObject SumLabel,
    xcF :: Morphism XConeObject SumLabel,
    xcG :: Morphism XConeObject SumLabel,
    xcH :: Morphism XConeObject SumLabel
  }
  deriving (Show)

xcone2 :: XCone2
xcone2 =
  XCone2
    { xcS = Morphism X (SumLabel 2) A,
      xcT = Morphism X (SumLabel 3) B,
      xcF = Morphism A (SumLabel 1) B,
      xcG = Morphism B (SumLabel (-1)) A,
      xcH = Morphism B (SumLabel 10) C
    }

instance Morphisms XConeObject SumLabel where
  morphisms =
    let (XCone2 s t f g h) = xcone2
     in [s, t, f, g, h]
