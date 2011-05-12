-- | Rank 2 functors.

{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.Functor2
  ( Functor2 (..)
  ) where

class Functor2 t where
  fmap2 :: (forall a . f a -> g a) -> t f -> t g
