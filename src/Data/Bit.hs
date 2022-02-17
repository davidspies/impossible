{-# LANGUAGE LambdaCase #-}

module Data.Bit (Bit (..), coerce) where

import Numeric.Natural (Natural)

data Bit = Zero | One
  deriving (Eq, Ord, Show, Enum)

coerce :: Bit -> Natural
coerce = \case
  Zero -> 0
  One -> 1
