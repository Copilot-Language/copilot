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
import qualified Copilot.Kind.Kind2Format as K2
import Copilot.Kind.Naive.Check           as Naive

import Grey

line = replicate 40 '-'

main :: IO ()
main =  do
  cspec <- reify spec
  -- putStrLn $ prettyPrint cspec
  -- putStrLn line
  -- putStrLn $ TS.prettyPrint . TS.complete . TS.removeCycles . TS.translate $ cspec
  -- putStrLn line 
  -- putStrLn $ K2.toKind2 . TS.translate $ cspec

  b <- Naive.check def cspec
  putStrLn line
  print b
  putStrLn line
  return ()


