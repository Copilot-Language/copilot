--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
module Main (main) where

import qualified Copilot.Core as Core
import Copilot.Language.Reify
import Copilot.Core.Interpret
import Copilot.Core.PrettyPrint

import qualified Copilot.Kind.TransSys    as TS
import qualified Copilot.Kind.Kind2       as K2
import Copilot.Kind.Naive.Check           as Naive

import Grey

line = replicate 40 '-'

main :: IO ()
main =  do
  cspec <- reify spec
  -- putStrLn $ prettyPrint cspec
  -- putStrLn line
  -- putStrLn $ TS.prettyPrint . TS.translate $ cspec
  --print $ TS.isTopologicallySorted tsys
  --print $ TS.invariants tsys
  -- print $ map TS.invariants (TS.specNodes tsys)
  ---putStrLn . TS.prettyPrint $ tsys
  -- putStrLn $ TS.prettyPrint . TS.complete . TS.removeCycles . TS.translate $ cspec
  putStrLn $ K2.prettyPrint . K2.toKind2 . TS.translate $ cspec

--  b <- Naive.check def cspec
--  putStrLn line
--  print b
--  putStrLn line
--  return ()


