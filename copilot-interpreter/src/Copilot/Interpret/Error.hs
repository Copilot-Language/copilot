-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE Safe #-}

-- | Custom functions to report error messages to users.
module Copilot.Interpret.Error
  ( badUsage ) where

-- | Report an error due to an error detected by Copilot (e.g., user error).
badUsage :: String -- ^ Description of the error.
         -> a
badUsage msg = error $ "Copilot error: " ++ msg
