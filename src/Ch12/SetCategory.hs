module Ch12.SetCategory
  ( SetMorphism (..),
    SetObject (..),
    (<.>),
    identity,
    integers,
    positiveIntegers,
    booleans,
    validate,
    singleton,
    emptySet,
    finiteSet,
    multiplesOf,
    multiply,
  )
where

import Data.List (partition)
import Text.Printf (printf)

data SetObject a = SetObject
  { -- | human-readable name
    sname :: String,
    -- | representative values---morphisms are considered equal if they produce identical outputs
    -- for all of these inputs
    samples :: [a],
    -- | true if the set contains this value
    contains :: (a -> Bool)
  }

finiteSet :: Eq a => String -> [a] -> SetObject a
finiteSet nm xs = SetObject nm xs (flip elem xs)

instance Eq (SetObject a) where
  SetObject _ as aok == SetObject _ bs bok = (all aok bs) && (all bok as)

instance Show (SetObject a) where
  show (SetObject name _ _) = name

data SetMorphism a b = SetMorphism
  { -- | the domain
    source :: SetObject a,
    -- | human-readable name
    mname :: String,
    -- | transforms inputs to outputs
    op :: a -> b,
    -- | the codomain
    dest :: SetObject b
  }

instance (Eq a, Eq b) => Eq (SetMorphism a b) where
  (SetMorphism sx _ opx dx) == (SetMorphism sy _ opy dy)
    | sx /= sy || dx /= dy = False
    | otherwise = fmap opx (samples sx) == fmap opy (samples sx)

instance (Show a, Show b) => Show (SetMorphism a b) where
  show (SetMorphism s name _ d) = printf "%s :: %s -> %s" name (show s) (show d)

-- | If there is at least one invalid sample, return a tuple with the valid and invalid samples
validate :: SetMorphism a b -> Either ([b], [b]) (SetMorphism a b)
validate m@(SetMorphism s _ f d) =
  let fromF = fmap f (samples s)
      isValid = (contains d)
      categorized = partition isValid fromF
   in if ((not . null . snd) categorized)
        then (Left categorized)
        else (Right m)

(<.>) :: SetMorphism b c -> SetMorphism a b -> SetMorphism a c
(SetMorphism sf nf opf df) <.> (SetMorphism sg ng opg dg)
  | dg /= sf = undefined
  | otherwise = SetMorphism sg (printf "%s . %s" nf ng) (opf . opg) df

booleans :: SetObject Bool
booleans = SetObject "Bool" [False, True] (const True)

identity :: SetObject a -> SetMorphism a a
identity x = SetMorphism x "id" id x

singleton :: SetObject ()
singleton = SetObject "Singleton" [()] (== ())

emptySet :: SetObject a
emptySet = SetObject "Empty" ([] :: [a]) (const False)

multiplesOf :: Int -> SetObject Int
multiplesOf n =
  let xs = do
        x <- [-10 .. 10]
        return (n * x)
      inSet x = x `rem` n == 0
   in SetObject (printf "Int%d" n) xs inSet

integers :: SetObject Int
integers = SetObject "Int" [-20 .. 20] (const True)

positiveIntegers :: SetObject Int
positiveIntegers = SetObject "Int" [1 .. 40] (> 0)

multiply :: Int -> SetMorphism Int Int
multiply n = SetMorphism integers (printf "(* %d)" n) (* n) (multiplesOf n)
