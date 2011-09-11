module Test where

import qualified Prelude as P

import Copilot.Language.Prelude
import Copilot.Language
import Copilot.Language.Reify (reify)

import qualified Copilot.Compile.SBV as S

nats :: Stream Word64
nats = [0] ++ nats + 1

alt :: Stream Bool
alt = [True] ++ not alt

alt2 :: Stream Word64
alt2 = [0,1,2] ++ alt2 + 1

alt3 :: Stream Bool
alt3 = [True,True,False] ++ alt3

fib :: Stream Word64
fib = [0, 1] ++ fib + drop 1 fib

fib' :: Stream Word64
fib' = [0, 1] ++ fib' + drop 1 fib

logic :: Stream Bool
logic = [True, False] ++ logic || drop 1 logic

sumExterns :: Stream Word64
sumExterns =
  let
    e1 = extern "e1"
    e2 = extern "e2"
  in
    e1 + e2 + e1


spec :: Spec
spec = do
  trigger "trig1" alt [ arg $ nats < 3
                      , arg sumExterns 
                      , arg logic
                      ]

specC :: IO ()
specC = reify spec >>= S.compile "test1" 
  

fibSpec :: Spec
fibSpec = do
  trigger "fib_out" true [arg fib]

fibC :: IO ()
fibC = reify fibSpec >>= S.compile "fib" 

