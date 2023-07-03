module Assertions.HUnit (assertValid) where

import Lib.SetCategory
import Test.HUnit

-- | Verifies that a morphism can transform all the samples from the input set to values
-- that appear in the output set
assertValid :: (Show b) => SetMorphism a b -> Assertion
assertValid f =
  case (validate f) of
    Left partitioned -> assertFailure $ (show . snd) partitioned
    Right _ -> return ()
