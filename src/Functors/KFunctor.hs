module Functors.KFunctor (K (..)) where

import Lib.SetFunctor

data K a = K a deriving (Show, Eq)

instance Functor K where
  fmap f (K x) = K (f x)

instance SetFunctor K where
  create = K
  extract (K x) = x
