module Ch20.MonoidStringConcat
  ( MonoidStringConcat (..),
  )
where

newtype MonoidStringConcat = MonoidStringConcat {mpvalue :: String} deriving (Eq, Ord, Show)

instance Semigroup MonoidStringConcat where
  (MonoidStringConcat l1) <> (MonoidStringConcat l2) = MonoidStringConcat (l1 ++ l2)

instance Monoid MonoidStringConcat where
  mempty = MonoidStringConcat ""
