module Ch14.AbcCategory
  ( AbcObject (..),
    AbcLabel (..),
  )
where

import Lib.Category
import Text.Printf

data AbcObject = A | B | C deriving (Show, Eq)

instance Objects AbcObject where
  objects = [A, B, C]

newtype AbcLabel = AbcLabel {abcLabel :: String} deriving (Eq, Ord, Show)

instance Semigroup AbcLabel where
  (AbcLabel l1) <> (AbcLabel l2) = AbcLabel $ printf "%s . %s" l1 l2

instance Morphisms AbcObject AbcLabel where
  morphismsFrom A = [(Morphism A (AbcLabel "1a") A), (Morphism A (AbcLabel "f") B)]
  morphismsFrom B = [(Morphism B (AbcLabel "1b") B), (Morphism B (AbcLabel "g") A)]
  morphismsFrom C = [Morphism C (AbcLabel "1c") C]
