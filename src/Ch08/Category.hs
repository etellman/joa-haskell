module Ch08.Category
  ( Morphism (..),
    Morphisms (..),
    Objects (..),
    SumLabel (..),
    isId,
    fromTo,
    findIn,
    sumIdentity,
    opposite,
  )
where

import Data.Semigroup

data Morphism o l = Morphism
  { source :: o,
    mlabel :: l,
    dest :: o
  }
  deriving (Show, Eq)

class Objects o where
  objects :: [o]

-- | Define at least one of morphisms or morphismsFrom
class (Eq o, Objects o) => Morphisms o l where
  -- | All the morphisms
  morphisms :: [Morphism o l]
  morphisms = concat $ fmap morphismsFrom objects

  -- | The morphisms with a particular object as the source
  morphismsFrom :: o -> [Morphism o l]
  morphismsFrom x =
    let matchesSource (Morphism s _ _) = s == x
     in filter matchesSource morphisms

  -- | The morphisms with a particular object as the dest
  morphismsTo :: o -> [Morphism o l]
  morphismsTo = withDest morphisms

  -- | Compose two morphisms
  (<.>) :: (Semigroup l) => Morphism o l -> Morphism o l -> Morphism o l
  (Morphism sf lf df) <.> (Morphism sg lg dg)
    | dg /= sf = undefined
    | otherwise = Morphism sg (lf <> lg) df

-- | the morphisms with dest x
withDest :: (Eq a) => [Morphism a b] -> a -> [Morphism a b]
withDest xs x = filter ((== x) . dest) xs

opposite :: (Morphisms o l) => [Morphism o l]
opposite =
  let flipMorphism (Morphism s l d) = Morphism d l s
   in map flipMorphism morphisms

fromTo :: (Morphisms o l) => [Morphism o l] -> o -> o -> [Morphism o l]
fromTo ms s d = filter (\x -> source x == s && dest x == d) ms

findIn :: (Morphisms o l) => [Morphism o l] -> o -> o -> Morphism o l
findIn ms x y = head $ fromTo ms x y

-- | determines whether a morphism is an identity
isId :: (Eq a) => Morphism a b -> Bool
isId (Morphism s _ d) = s == d

newtype SumLabel = SumLabel {sumLabel :: (Sum Int)} deriving (Eq, Show)

instance Semigroup SumLabel where
  (SumLabel x) <> (SumLabel y) = SumLabel (x <> y)

sumIdentity :: o -> Morphism o SumLabel
sumIdentity x = Morphism x (SumLabel 0) x
