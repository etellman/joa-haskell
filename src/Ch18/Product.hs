module Ch18.Product
  ( productMorphism,
    productSet,
  )
where

import Ch12.SetCategory
import Text.Printf

productMorphism ::
  SetMorphism a b ->
  SetMorphism a c ->
  SetObject (b, c) ->
  SetMorphism a (b, c)
productMorphism (SetMorphism fs fn fop _) (SetMorphism gs gn gop _) d
  | fs /= gs = undefined
  | otherwise =
      let pname = printf "(%s x %s)" fn gn
          pop x = (fop x, gop x)
       in SetMorphism fs pname pop d

productSet :: SetObject a -> SetObject b -> SetObject (a, b)
productSet xs ys =
  let xys = do
        x <- samples xs
        y <- samples ys
        return (x, y)
   in SetObject "A x B" xys (const True)
