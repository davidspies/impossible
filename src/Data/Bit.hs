module Data.Bit (Bit (..)) where

import Numeric.Natural (Natural)

data Bit = Zero | One
  deriving (Eq, Show)
