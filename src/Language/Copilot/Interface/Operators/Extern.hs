-- |

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Operators.Extern
  ( CName
  , Extern (..)
  ) where

type CName = String

class Extern β α | α → β where
  extern ∷ CName → α β
