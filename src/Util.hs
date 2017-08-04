{-# LANGUAGE LambdaCase #-}

module Util where

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = \case
  []          -> []
  Nothing : _ -> []
  Just x : xs -> x : takeWhileJust xs

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f = takeWhileJust . iterate (>>= f) . Just
