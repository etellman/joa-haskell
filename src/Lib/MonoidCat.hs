module Lib.MonoidCat (MonoidObject (..)) where

import Lib.Category

data MonoidObject = MonoidObject deriving (Eq, Show)

instance Objects MonoidObject where
  objects = [MonoidObject]
