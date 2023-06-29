module Ch23.RepresentableFunctor
  ( liftH,
    liftH',
  )
where

import Ch08.Category
import qualified Ch12.SetCategory as SC
import Text.Printf

homSet ::
  (Eq l, Show o, Morphisms o l) =>
  o ->
  o ->
  SC.SetObject (Morphism o l)
homSet x a =
  let l = printf "C(%s, %s)" (show x) (show a)
      xs = filter ((== a) . dest) (morphismsFrom x)
   in SC.finiteSet l xs

-- | lift a representable functor
liftH ::
  (Eq l, Show l, Show o, Semigroup l, Morphisms o l) =>
  o ->
  Morphism o l ->
  SC.SetMorphism (Morphism o l) (Morphism o l)
liftH x f@(Morphism s l d) =
  let l' = printf "%s . _" (show l)
   in SC.SetMorphism (homSet x s) l' (f <.>) (homSet x d)

-- | lift a representable functor - dual version
liftH' ::
  (Eq l, Show l, Show o, Semigroup l, Morphisms o l) =>
  o ->
  Morphism o l ->
  SC.SetMorphism (Morphism o l) (Morphism o l)
liftH' x f@(Morphism s l d) =
  let l' = printf "_ . %s" (show l)
   in SC.SetMorphism (homSet d x) l' (<.> f) (homSet s x)
