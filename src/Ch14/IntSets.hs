module Ch14.IntSets
  ( f12,
    f21,
    f14,
    f41,
    f24,
    f42,
  )
where

import Lib.SetCategory

-- This is a category with 3 set objects: all integers, even integers and multiples of 4.
-- The objects are all isomorphic with each other since you can go to/from any two objects by
-- multiplying or dividing by 2

f12 :: SetMorphism Int Int
f12 = SetMorphism integers "(* 2)" (* 2) (multiplesOf 2)

f21 :: SetMorphism Int Int
f21 = SetMorphism (multiplesOf 2) "(/ 2)" (flip div 2) integers

f14 :: SetMorphism Int Int
f14 = SetMorphism integers "(* 4)" (* 4) (multiplesOf 4)

f41 :: SetMorphism Int Int
f41 = SetMorphism (multiplesOf 4) "(/ 4)" (flip div 4) integers

f24 :: SetMorphism Int Int
f24 = SetMorphism (multiplesOf 2) "(* 2)" (* 2) (multiplesOf 4)

f42 :: SetMorphism Int Int
f42 = SetMorphism (multiplesOf 4) "(/ 2)" (flip div 2) (multiplesOf 2)
