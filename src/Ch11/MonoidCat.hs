module Ch11.MonoidCat (MonoidObject (..)) where

import Ch08.Category

data MonoidObject = MonoidObject deriving (Eq, Show)

instance Objects MonoidObject where
  objects = [MonoidObject]
