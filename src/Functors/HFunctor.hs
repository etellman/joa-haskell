module Functors.HFunctor (H (..)) where

import Lib.SetFunctor

data H a = H a deriving (Show, Eq)

instance Functor H where
  fmap f (H x) = H (f x)

instance SetFunctor H where
  create = H
  extract (H x) = x
