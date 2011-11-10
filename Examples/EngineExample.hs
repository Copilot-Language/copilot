--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import qualified Copilot.Compile.SBV as S
import qualified Prelude as P

{- 
  "If the majority of the engine temperature probes exeeds 250 degrees, then the
  cooler is engaged and remains engaged until the majority of the engine
  temperature probes drop to 250 or below.  Otherwise, trigger an immediate
  shutdown of the engine."  -}

engineMonitor :: Spec
engineMonitor = do
  trigger "shutoff" (not ok) [arg maj]
  where
  vals     = map externW8 ["tmp_probe_0", "tmp_probe_1", "tmp_probe_2"]
  exceed   = map (> 250) vals
  maj      = majority exceed
  checkMaj = aMajority exceed maj
  ok       = alwaysBeen ((maj && checkMaj) ==> extern "cooler") 

main :: IO ()
main = 
  interpret 
    10
    [ var "cooler" cooler, var "tmp_probe_0" two51
    , var "tmp_probe_1" two51, var "tmp_probe_2" zero]
    engineMonitor
  where
  two51 = [251, 251] P.++ repeat (250 :: Word8)
  zero  = repeat (0 :: Word8)
  cooler = [True, True] P.++ repeat False

--  reify engineMonitor >>= S.compile (S.Params { S.prefix = Just "engine" })
  
    
