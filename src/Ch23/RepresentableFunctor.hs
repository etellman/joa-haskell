module Ch23.RepresentableFunctor (liftH) where

import Ch08.Category
import qualified Ch12.SetCategory as SC
import Text.Printf

-- | lift a representable functor
liftH ::
  (Eq l, Show l, Show o, Semigroup l, Morphisms o l) =>
  o ->
  Morphism o l ->
  SC.SetMorphism (Morphism o l) (Morphism o l)
liftH x f@(Morphism s l d) =
  let homSet x' y =
        SC.finiteSet
          (printf "C(%s, %s)" (show x') (show y))
          (filter ((== y) . source) (morphismsFrom x'))
      l' = printf "%s . _" (show l)
   in SC.SetMorphism (homSet x s) l' (f <.>) (homSet x d)
