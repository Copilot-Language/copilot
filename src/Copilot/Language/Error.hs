--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | Custom functions to report error messages to users.
module Copilot.Language.Error
  ( impossible
  , badUsage ) where

-- | Report an error due to a bug in Copilot.
impossible :: String -- ^ Name of the function in which the error was detected.
           -> String -- ^ Name of the package in which the function is located.
           -> a
impossible function package =
  error $ "Impossible error in function " ++ function ++ ", in package " ++
    package ++ ".  Please email Lee Pike at <lee pike @ gmail . com> " ++
    "(remove spaces) or file a bug report on github.com."

-- | Report an error due to an error detected by Copilot (e.g., user error).
badUsage :: String -- ^ Description of the error.
         -> a
badUsage msg = error $ "Copilot error: " ++ msg
