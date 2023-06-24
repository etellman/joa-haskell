module Ch11.MonoidPlusSet
  ( MonoidPlus (..),
  )
where

newtype MonoidPlus = MonoidPlus {mpvalue :: Int} deriving (Eq, Ord, Show)

instance Semigroup MonoidPlus where
  (MonoidPlus l1) <> (MonoidPlus l2) = MonoidPlus (l1 + l2)

instance Monoid MonoidPlus where
  mempty = MonoidPlus 0
