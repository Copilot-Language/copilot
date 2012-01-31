--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example in sampling external functions.

{-# LANGUAGE RebindableSyntax #-}

module ExtFuns ( extFuns ) where

import Language.Copilot 
import qualified Copilot.Compile.C99 as C
import qualified Copilot.Compile.SBV as S

--------------------------------------------------------------------------------

nats :: Stream Word16
nats = [0,1] ++ nats + 1

---------------------------------------------------------------------------------

-- Function func0 and it's environment for interpreting
func0 :: Stream Word16
func0 = externFun "func0" [ arg x, arg nats ]
          (Just $ cast x + nats)
  where x = externW8 "x" (Just [0..])

---------------------------------------------------------------------------------

-- Function func1 and it's environment for interpreting
func1 :: Stream Bool
func1 = externFun "func1" [] (Just $ [False] ++ not func1)
          
---------------------------------------------------------------------------------

-- Function func0 with another set of args and it's environment for interpreting
func2 :: Stream Word16
func2 = externFun "func0" [ arg $ constW8 3, arg $ constW16 4 ]
          (Just $ cast (constW8 3) + constW16 4)

---------------------------------------------------------------------------------

a :: Stream Word16
a = func0 + func0

spec :: Spec
spec = trigger "trigger" true [ arg func0
                              , arg func1
                              , arg func2
                              , arg a ]
  
extFuns :: IO ()
extFuns = do
   interpret 10 spec
   reify spec >>= C.compile C.defaultParams 
   reify spec >>= S.compile S.defaultParams 


--------------------------------------------------------------------------------
