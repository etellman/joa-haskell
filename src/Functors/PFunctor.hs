module Functors.PFunctor (P (..)) where

import Lib.SetFunctor

data P a = P a deriving (Show, Eq)

instance Functor P where
  fmap f (P x) = P (f x)

instance SetFunctor P where
  create = P
  extract (P x) = x
