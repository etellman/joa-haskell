module Functors.RFunctor (R (..)) where

import Ch20.SetFunctor

data R a = R a deriving (Show, Eq)

instance Functor R where
  fmap f (R x) = R (f x)

instance SetFunctor R where
  create = R
  extract (R x) = x
