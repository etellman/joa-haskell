module Ch20.SetFunctor
  ( natural,
    lift,
    SetFunctor (..),
  )
where

import Ch12.SetCategory
import Text.Printf

class (Functor m) => SetFunctor m where
  create :: a -> m a
  extract :: m a -> a

-- | Lift a morphism from one category to another
lift :: (SetFunctor m) => SetMorphism a b -> SetMorphism (m a) (m b)
lift (SetMorphism s name f d) =
  let liftObj (SetObject nm xs ok) =
        SetObject
          (printf "lift (%s)" nm)
          (fmap create xs)
          (extract . fmap ok)
   in SetMorphism (liftObj s) (printf "lift (%s)" name) (fmap f) (liftObj d)

-- | The natural transformation of an object from one category to another
natural ::
  (SetFunctor m, SetFunctor n) =>
  SetObject (m a) ->
  SetMorphism (m a) (n a)
natural x =
  let transform = create . extract
      naturalObj (SetObject nm xs ok) =
        SetObject
          (printf "natural (%s)" nm)
          (fmap transform xs)
          (extract . fmap (ok . create))
   in SetMorphism x "natural" transform (naturalObj x)
