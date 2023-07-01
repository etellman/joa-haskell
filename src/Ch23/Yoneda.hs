module Ch23.Yoneda
  ( naturalH,
  )
where

import Ch08.Category
import qualified Ch12.SetCategory as SC
import Text.Printf

naturalH ::
  (Semigroup l, Morphisms o l) =>
  SC.SetObject (Morphism o l) ->
  Morphism o l ->
  SC.SetMorphism (Morphism o l) (Morphism o l)
naturalH x f =
  let transform = (f <.>)
      naturalObj (SC.SetObject nm fs ok) =
        SC.SetObject
          (printf "natural (%s)" nm)
          (fmap transform fs)
          (\g -> ok $ f <.> g)
   in SC.SetMorphism x "natural" transform (naturalObj x)
