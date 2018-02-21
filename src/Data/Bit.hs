{-# LANGUAGE LambdaCase #-}

module Data.Bit
    ( Bit (..)
    , coerce
    ) where

import Data.Natural

data Bit = Zero | One
  deriving (Eq, Ord, Show, Enum)

coerce :: Bit -> Natural
coerce = \case
  Zero -> 0
  One  -> 1
