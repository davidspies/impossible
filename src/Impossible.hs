module Impossible where

import Data.Bit (Bit (One, Zero))
import Data.Cantor
import Data.Functor ((<&>))
import Data.List (scanl)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Util (binSearch, takeWhileJust)
import Prelude hiding (drop)

relevant :: Eq a => (Cantor -> a) -> [Natural]
relevant f = result
  where
    result = takeWhileJust $ go <$> scanl (flip Set.insert) Set.empty result
    go known =
      find isCounterExample <&> \x ->
        binSearch (\n -> isCounterExample (splice n x (x ! n) (retainOnly known x)))
      where
        isCounterExample x = f x /= f (retainOnly known x)

modulus :: Eq a => (Cantor -> a) -> Natural
modulus f = binSearch $ \n ->
  not $ exists $ \x -> f x /= f (splice n x Zero zeros)
