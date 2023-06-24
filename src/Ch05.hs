module Ch05
  ( mod4add,
  )
where

-- | morphisms are things like (mod4add 3), (mod4add 17), etc.
-- domain: integers
-- co-domain: 0, 1, 2, 3
mod4add :: Int -> Int -> Int
mod4add x y = (x + y) `mod` 4
