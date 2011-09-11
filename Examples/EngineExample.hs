--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import qualified Copilot.Compile.SBV as S

{- 
  "If the majority of the engine temperature probes exeeds 250 degrees, then the
  cooler is engaged and remains engaged until the majority of the engine
  temperature drops to 250 or below.  Otherwise, trigger an immediate shutdown
  of the engine."  -}

engineMonitor :: Spec
engineMonitor = do
  trigger "shutoff" (not overHeat) [arg maj]
  where
  vals     = map externW8 ["tmp_probe_0", "tmp_probe_1", "tmp_probe_2"]
  exceed   = map (< 250) vals
  maj      = majority exceed
  checkMaj = aMajority exceed maj
  overHeat = (extern "cooler" || (maj && checkMaj)) `since` not maj

main :: IO ()
main = 
  reify engineMonitor >>= S.compile (S.Params { S.prefix = Just "engine" })
  
    
