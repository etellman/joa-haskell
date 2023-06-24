module Functors.FFunctor
  ( F (..),
    intFs,
  )
where

import Ch12.SetCategory
import Ch20.SetFunctor

data F a = F a deriving (Show, Eq)

instance Functor F where
  fmap f (F x) = F (f x)

instance SetFunctor F where
  create = F
  extract (F x) = x

intFs :: SetObject (F Int)
intFs = SetObject "F Int" (fmap F [-20 .. 20]) (const True)
