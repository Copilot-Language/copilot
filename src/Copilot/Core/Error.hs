--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

module Copilot.Core.Error
  ( impossible
  , badUsage ) where

impossible :: String -> String -> a
impossible function package =
  error $ "\"Impossible\" error in function "
    ++ function ++ ", in package " ++ package
    ++ ". Please file an issue at "
    ++ "https://github.com/Copilot-Language/copilot/issues"
    ++ "or email the maintainers at <dev@dedden.net>"

badUsage :: String -> a
badUsage msg = error $ "Copilot error: " ++ msg
