module Ch11.MonoidMod4Set (MonoidMod4 (..)) where

newtype MonoidMod4 = MonoidMod4 {m4value :: Int} deriving (Eq, Ord, Show)

instance Semigroup MonoidMod4 where
  (MonoidMod4 l1) <> (MonoidMod4 l2) = MonoidMod4 $ (l1 + l2) `rem` 4

instance Monoid MonoidMod4 where
  mempty = MonoidMod4 0
