-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE Safe #-}

-- | Custom functions to report error messages to users.
module Copilot.Core.Error
  ( impossible
  , badUsage ) where

-- | Report an error due to a bug in Copilot.
impossible :: String -- ^ Name of the function in which the error was detected.
           -> String -- ^ Name of the package in which the function is located.
           -> a
impossible function package =
  error $ "\"Impossible\" error in function "
    ++ function ++ ", in package " ++ package
    ++ ". Please file an issue at "
    ++ "https://github.com/Copilot-Language/copilot/issues"
    ++ "or email the maintainers at <ivan.perezdominguez@nasa.gov>"

-- | Report an error due to an error detected by Copilot (e.g., user error).
badUsage :: String -- ^ Description of the error.
         -> a
badUsage msg = error $ "Copilot error: " ++ msg
