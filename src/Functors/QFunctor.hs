module Functors.QFunctor (Q (..)) where

import Lib.SetFunctor

data Q a = Q a deriving (Show, Eq)

instance Functor Q where
  fmap f (Q x) = Q (f x)

instance SetFunctor Q where
  create = Q
  extract (Q x) = x
