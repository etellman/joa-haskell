module Functors.HFunctor
  ( H (..),
    intHs,
  )
where

import Ch12.SetCategory
import Ch20.SetFunctor

data H a = H a deriving (Show, Eq)

instance Functor H where
  fmap f (H x) = H (f x)

instance SetFunctor H where
  create = H
  extract (H x) = x

intHs :: SetObject (H Int)
intHs = SetObject "H Int" (fmap H [-20 .. 20]) (const True)
