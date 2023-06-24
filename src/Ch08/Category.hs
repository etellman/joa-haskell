module Ch08.Category
  ( Morphism (..),
    Morphisms (..),
    Objects (..),
    isId,
    withdest,
  )
where

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
  morphismsTo = withdest morphisms

  -- | Compose two morphisms
  (<.>) :: (Semigroup l) => Morphism o l -> Morphism o l -> Morphism o l
  (Morphism sf lf df) <.> (Morphism sg lg dg)
    | dg /= sf = undefined
    | otherwise = Morphism sg (lf <> lg) df

-- | the morphisms with dest x
withdest :: (Eq a) => [Morphism a b] -> a -> [Morphism a b]
withdest xs x = filter ((== x) . dest) xs

-- | determines whether a morphism is an identity
isId :: (Eq a) => Morphism a b -> Bool
isId (Morphism s _ d) = s == d
