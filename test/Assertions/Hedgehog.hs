module Assertions.Hedgehog
  ( assertValid,
    (==>),
    (<==>),
  )
where

import Ch12.SetCategory
import Hedgehog

-- | Verifies that a morphism can transform all the samples from the input set to values
-- that appear in the output set
assertValid :: (Show b) => SetMorphism a b -> PropertyT IO (SetMorphism a b)
assertValid = evalEither . validate

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>

(<==>) :: MonadTest m => Bool -> Bool -> m ()
(<==>) a b = assert $ a && b || not a && not b

infixr 0 <==>
