module Impossible where

import BinSearch (binSearch)
import Data.Cantor
import Data.Functor ((<&>))
import Data.List (scanl)
import qualified Data.Set as Set
import Numeric.Natural
import Util
import Prelude hiding (drop)

relevant :: Eq a => (Cantor -> a) -> [Natural]
relevant f = result
  where
    result = takeWhileJust $ map go $ scanl (flip Set.insert) Set.empty result
    go known =
      find isCounterExample <&> \x ->
        binSearch (\n -> isCounterExample $ prependFrom n x (drop n $ retainOnly known x)) - 1
      where
        isCounterExample x = f x /= f (retainOnly known x)

modulus :: Eq a => (Cantor -> a) -> Natural
modulus f = last $ repeatedly go 0
  where
    go :: Natural -> Maybe Natural
    go n = (<$> find isCounterExample) $ \x ->
      binSearch (\k -> isCounterExample $ prependFrom (k + n) x zeros) + n
      where
        isCounterExample x = f x /= f (prependFrom n x zeros)
