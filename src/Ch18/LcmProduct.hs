module Ch18.LcmProduct
  ( commonSource,
    minCommonSource,
  )
where

import Ch08.Category
import Ch08.Factor30
import Data.Function
import Data.List (minimumBy)

-- | morphisms to x and y with a common source
commonSource :: Factor30 -> Factor30 -> [Factor30]
commonSource x y =
  let to = morphismsTo :: Factor30 -> [Morphism Factor30 Factor30Label]
      matchingY xMorphism = any (\yMorphism -> source yMorphism == source xMorphism) (to y)
   in map source (filter matchingY (to x))

minCommonSource :: Factor30 -> Factor30 -> Factor30
minCommonSource x y = minimumBy (compare `on` f30factor) (commonSource x y)
