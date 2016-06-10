--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

module Copilot.Language.Error
  ( impossible
  , badUsage ) where

impossible :: String -> String -> a
impossible function package =
  error $ "Impossible error in function " ++ function ++ ", in package " ++
    package ++ ".  Please email Lee Pike at <lee pike @ gmail . com> " ++
    "(remove spaces) or file a bug report on github.com."

badUsage :: String -> a
badUsage msg = error $ "Copilot error: " ++ msg
