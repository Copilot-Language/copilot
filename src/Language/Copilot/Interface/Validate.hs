-- |

{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Interface.Validate
  ( validate
  ) where

import Language.Copilot.Core (Specification)

validate :: Specification spec => spec a -> Maybe String
validate = undefined
