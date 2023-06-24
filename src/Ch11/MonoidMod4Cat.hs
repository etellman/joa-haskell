module Ch11.MonoidMod4Cat
  ( Mod4Label (..),
    mod4Morphism,
  )
where

import Ch08.Category
import Ch11.MonoidCat

newtype Mod4Label = Mod4Label {m4label :: Int} deriving (Eq, Ord, Show)

instance Semigroup Mod4Label where
  (Mod4Label l1) <> (Mod4Label l2) = Mod4Label $ (l1 + l2) `rem` 4

instance Morphisms MonoidObject Mod4Label where
  morphisms = fmap mod4Morphism [0 .. 3]

mod4Morphism :: Int -> Morphism MonoidObject Mod4Label
mod4Morphism l = Morphism MonoidObject (Mod4Label l) MonoidObject
