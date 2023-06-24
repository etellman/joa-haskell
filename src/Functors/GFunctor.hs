module Functors.GFunctor
  ( G (..),
    intGs,
  )
where

import Ch12.SetCategory
import Ch20.SetFunctor

data G a = G a deriving (Show, Eq)

instance Functor G where
  fmap f (G x) = G (f x)

instance SetFunctor G where
  create = G
  extract (G x) = x

intGs :: SetObject (G Int)
intGs = SetObject "G Int" (fmap G [-20 .. 20]) (const True)
