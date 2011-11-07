--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example in sampling external functions.

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot 
import qualified Copilot.Compile.C99 as C
--------------------------------------------------------------------------------

nats :: Stream Word16
nats = [0] ++ nats + 1

func0 :: Stream Word16
func0 = externFun "func0" [funArg $ externW8 "x", funArg nats]

-- A reference implementation of "func0" for simulation.
func0Spec :: Stream Word16
func0Spec = cast (externW8 "x") + nats

func1 :: Stream Bool
func1 = externFun "func1" []

-- A reference implementation of "func0" for simulation.
func1Spec :: Stream Bool
func1Spec = [False] ++ not func1

spec :: Spec
spec = trigger "trigger" true [ arg func0, arg func1 ]
  
main :: IO ()
main = 
--  reify spec >>= C.compile C.defaultParams 
    interpret 10 [ func "func0" func0Spec
                 , var "x" ([0..] :: [Word8]) 
                 , func "func1" func1Spec
                 ] 
              spec

--------------------------------------------------------------------------------
