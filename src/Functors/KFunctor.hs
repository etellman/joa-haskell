module Functors.KFunctor
  ( K (..),
    intKs,
  )
where

import Ch12.SetCategory
import Ch20.SetFunctor

data K a = K a deriving (Show, Eq)

instance Functor K where
  fmap f (K x) = K (f x)

instance SetFunctor K where
  create = K
  extract (K x) = x

intKs :: SetObject (K Int)
intKs = SetObject "K Int" (fmap K [-20 .. 20]) (const True)
