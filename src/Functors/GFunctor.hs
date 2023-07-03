module Functors.GFunctor (G (..)) where

import Lib.SetFunctor

data G a = G a deriving (Show, Eq)

instance Functor G where
  fmap f (G x) = G (f x)

instance SetFunctor G where
  create = G
  extract (G x) = x
