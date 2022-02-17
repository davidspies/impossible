{-# LANGUAGE LambdaCase #-}

module Util where

import Numeric.Natural (Natural)

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = \case
  [] -> []
  Nothing : _ -> []
  Just x : xs -> x : takeWhileJust xs

-- Returns some n such that (pred (n - 1)) is False and (pred n) is True
-- Assumes exists M such that for all m > M, (pred m) is True
-- Returns 0 if pred 0 is True
binSearch :: (Natural -> Bool) -> Natural
binSearch p
  | p 0 = 0
  | p 1 = 1
  | otherwise = binSearchOnBounds $ findBoundsFrom 1
  where
    binSearchOnBounds (lo, hi)
      | hi - lo == 1 = hi
      | p mid = binSearchOnBounds (lo, mid)
      | otherwise = binSearchOnBounds (mid, hi)
      where
        mid = (lo + hi) `quot` 2
    findBoundsFrom prevUp
      | p nextUp = (prevUp, nextUp)
      | otherwise = findBoundsFrom nextUp
      where
        nextUp = prevUp * 2
