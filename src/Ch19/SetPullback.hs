module Ch19.SetPullback (pullbackSet) where

import Lib.SetCategory
import Control.Monad

-- import Text.Printf

pullbackSet ::
  Eq c =>
  SetObject a ->
  SetObject b ->
  (SetMorphism a c) ->
  (SetMorphism b c) ->
  SetObject (a, b)
pullbackSet xs ys s t =
  let isValid (x, y) = op s x == op t y
      xys = do
        x <- samples xs
        y <- samples ys
        guard (isValid (x, y))
        return (x, y)
   in SetObject "A xc B" xys isValid
