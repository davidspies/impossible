{-# LANGUAGE FlexibleInstances #-}

module Data.Cantor
  ( Cantor (..),
    Predicate,
    exists,
    find,
    forall,
    splice,
    retainOnly,
    search,
    zeros,
  )
where

import Data.Bit
import Data.Function.Memoize (memoize)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural
import Prelude hiding (drop, pred, take)

newtype Cantor = Cantor {(!) :: Natural -> Bit}

splice :: Natural -> Cantor -> Bit -> Cantor -> Cantor
splice i x v y = Cantor $ \j -> case compare j i of
  LT -> x ! j
  EQ -> v
  GT -> y ! j

type Predicate = Cantor -> Bool

search :: Predicate -> Cantor
search pred = result
  where
    result = Cantor $
      memoize $ \i -> if exists $ pred . splice i result Zero then Zero else One

exists :: Predicate -> Bool
exists pred = pred $ search pred

find :: Predicate -> Maybe Cantor
find pred
  | pred result = Just result
  | otherwise = Nothing
  where
    result = search pred

forall :: Predicate -> Bool
forall = not . exists . (not .)

instance Eq a => Eq (Cantor -> a) where
  (==) f g = forall $ \x -> f x == g x

zeros :: Cantor
zeros = Cantor $ const Zero

retainOnly :: Set Natural -> Cantor -> Cantor
retainOnly nats xs = Cantor $ \i -> if i `Set.member` nats then xs ! i else Zero
