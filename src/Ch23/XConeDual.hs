module Ch23.XConeDual
  ( XConeDualObject (..),
    SumLabel (..),
    xconeDual2,
    XConeDual2 (..),
  )
where

import Ch08.Category

data XConeDualObject = A' | B' | C' | X' deriving (Eq, Show)

instance Objects XConeDualObject where
  objects = [A', B', C', X']

data XConeDual2 = XConeDual2
  { xcdS :: Morphism XConeDualObject SumLabel,
    xcdT :: Morphism XConeDualObject SumLabel,
    xcdF :: Morphism XConeDualObject SumLabel,
    xcdG :: Morphism XConeDualObject SumLabel,
    xcdH :: Morphism XConeDualObject SumLabel
  }
  deriving (Show)

xconeDual2 :: XConeDual2
xconeDual2 =
  XConeDual2
    { xcdS = Morphism A' (SumLabel 3) X',
      xcdT = Morphism B' (SumLabel 2) X',
      xcdF = Morphism A' (SumLabel 1) B',
      xcdG = Morphism B' (SumLabel (-1)) A',
      xcdH = Morphism B' (SumLabel 10) C'
    }

instance Morphisms XConeDualObject SumLabel where
  morphisms =
    let (XConeDual2 s t f g h) = xconeDual2
     in [s, t, f, g, h]
