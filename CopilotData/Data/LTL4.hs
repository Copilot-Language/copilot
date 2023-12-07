{-# LANGUAGE RankNTypes #-}
module CopilotData.Data.LTL4 where
import           Control.Monad
import           Copilot.Compile.C99
import qualified Data.Graph          as G
import           Data.List           ( intersect, lookup, nub, sort )
import           Data.Maybe          ( catMaybes, fromJust )
import           Data.Tuple          ( swap )
import           Debug.Trace
import           Language.Copilot    hiding ( not )
import           Prelude             hiding ( mod, (&&), (++), (/=), (<), (==),
                                       (>) )
import qualified Prelude             as P

type Result = Int8

absTrue :: Result
absTrue = 2

soFarTrue :: Result
soFarTrue = 1

soFarFalse :: Result
soFarFalse = -1

absFalse :: Result
absFalse = -2

negate' :: Stream Result -> Stream Result
negate' s = 0 - s

and' :: Stream Result -> Stream Result -> Stream Result
and' s1 s2 = mux (s1 < s2) s1 s2

or' :: Stream Result -> Stream Result -> Stream Result
or' s1 s2 = mux (s1 > s2) s1 s2

implies' :: Stream Result -> Stream Result -> Stream Result
implies' s1 s2 = negate' s1 `or'` s2

globally' :: Stream Result -> Stream Result
globally' s = mux (s > 0) (constant soFarTrue) s

eventually' :: Stream Result -> Stream Result
eventually' s = go
  where
    go     = mux (s == 2) s
--         $ mux (s == 1) s
           $ before
    before = [soFarFalse] ++ go

example :: Stream Result
example = extern "example" (Just [absTrue, absTrue, absFalse])

example2 :: Stream Result
example2 = extern "example" (Just [absFalse, absTrue, absFalse])

example3 :: Stream Result
example3 = extern "example" (Just [absFalse, absTrue, absTrue])

-- spec :: Spec
-- spec = do
--   observer "globally"  (globally' example2)
--   observer "example 1" (eventually' example2)
--   observer "example 2" (eventually' (globally' example2))
--   observer "example 3" (eventually' (globally' example3))

-- main :: IO ()
-- main = do
--   spec' <- reify spec
--   interpret 3 spec

data Prop = PConst Result
          | PStream (Stream Bool)
          | PNot Prop
          | PAnd Prop Prop
          | POr Prop Prop
          | PImplies Prop Prop
          | PGlobally Prop
          | PEventually Prop

eval :: Prop -> Stream Result
eval (PConst r)       = constant r
eval (PNot r)         = negate' (eval r)
eval (PAnd r1 r2)     = and' (eval r1) (eval r2)
eval (POr  r1 r2)     = or' (eval r1) (eval r2)
eval (PImplies r1 r2) = implies' (eval r1) (eval r2)
eval (PGlobally r)    = globally' (eval r)
eval (PEventually r)  = eventually' (eval r)
