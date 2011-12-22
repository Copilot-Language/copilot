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
func0 = externFun "func0" [ funArg $ externW8 "x"
                          , funArg nats]

func1 :: Stream Bool
func1 = externFun "func1" []

func2 :: Stream Word16
func2 = externFun "func0" [ funArg $ constW8 3
                          , funArg $ constW16 4]

a :: Stream Word16
a = func0 + func0

spec :: Spec
spec = trigger "trigger" true [ arg func0
                              , arg func1
                              , arg a ]
  
main :: IO ()
main = do
    reify spec >>= C.compile C.defaultParams 
    -- Reference implementations of our functions
    interpret 10 [ func "func0" (cast (externW8 "x") + nats)
                 , var "x" ([0..] :: [Word8]) 
                 , func "func1" ([False] ++ not func1)
                 ] 
              spec

--------------------------------------------------------------------------------
