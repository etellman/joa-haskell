module Ch18.SetCoproduct
  ( coproductSet,
    coproductMorphism,
    CoproductValue (..),
  )
where

import Ch12.SetCategory
import Text.Printf

data CoproductValue a b = A a | B b deriving (Show, Eq)

coproductOp :: (a -> c) -> (b -> c) -> CoproductValue a b -> c
coproductOp f _ (A x) = f x
coproductOp _ g (B x) = g x

coproductMorphism ::
  SetMorphism a c ->
  SetMorphism b c ->
  SetObject (CoproductValue a b) ->
  SetMorphism (CoproductValue a b) c
coproductMorphism (SetMorphism _ fn fop fd) (SetMorphism _ gn gop gd) sumObj
  | fd /= gd = undefined
  | otherwise =
      let name = printf "(%s + %s)" fn gn
       in SetMorphism sumObj name (coproductOp fop gop) gd

coproductSet :: SetObject a -> SetObject b -> SetObject (CoproductValue a b)
coproductSet xs ys =
  let as = map A (samples xs)
      bs = map B (samples ys)
   in SetObject "A + B" (as ++ bs) (const True)
