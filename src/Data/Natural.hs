{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Natural
    ( Natural
    , binSearch
    ) where

import           Data.Coerce   (coerce)
import           Data.MemoTrie (HasTrie (..))
import           Prelude

newtype Natural = Natural Integer
  deriving (Eq, Ord, Integral, Real)

instance Show Natural where
  show = coerce $ show @Integer

instance Num Natural where
  (+) = coerce $ (+) @Integer
  (*) = coerce $ (*) @Integer
  abs = id
  signum = coerce $ signum @Integer
  (-) (Natural x) (Natural y) = fromInteger $ x - y
  fromInteger n
    | n < 0 = error "Nat cannot be negative"
    | otherwise = Natural n

instance Enum Natural where
  succ = coerce $ succ @Integer
  pred (Natural n) = case n of
    0 -> error "Nat cannot be negative"
    _ -> Natural $ pred n
  toEnum = fromInteger . toEnum
  fromEnum = coerce $ fromEnum @Integer
  enumFrom = coerce $ enumFrom @Integer
  enumFromThen a b
    | b < a = [a,b..0]
    | otherwise = coerce (enumFromThen @Integer) a b
  enumFromThenTo = coerce $ enumFromThenTo @Integer

instance HasTrie Natural where
  newtype (:->:) Natural a = NaturalTrie (Integer :->: a)
  trie = coerce $ trie @Integer
  untrie = coerce $ untrie @Integer
  enumerate = coerce $ filter ((>= 0) . fst) . enumerate @Integer

-- Avoid "defined but not used" warning for NaturalTrie
_nt :: (Integer :->: a) -> (Natural :->: a)
_nt = NaturalTrie

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
