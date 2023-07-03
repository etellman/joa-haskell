module Ch23.Yoneda
  ( naturalH,
  )
where

import Ch08.Category
import qualified Ch12.SetCategory as SC
import Text.Printf

naturalH ::
  (Semigroup l, Show l, Eq l, Show o, Morphisms o l) =>
  Morphism o l ->
  SC.SetObject (Morphism o l) ->
  SC.SetMorphism (Morphism o l) (Morphism o l)
naturalH f x =
  let transform = (f <.>)
      name = (printf "naturalH %s %s" (show x) ((show . mlabel) f))
      naturalObj (SC.SetObject _ fs _) =
        SC.finiteSet (printf "%s" name) (fmap transform fs)
   in SC.SetMorphism x name transform (naturalObj x)
