{-# LANGUAGE RebindableSyntax #-}

module SerialBoyerMoore where

import Copilot.Language
import Copilot.Theorem
import Copilot.Theorem.Prover.Z3

import Prelude ()
import Data.String (fromString)

conj :: [Stream Bool] -> Stream Bool
conj = foldl (&&) true

disj :: [Stream Bool] -> Stream Bool
disj = foldl (||) false

forAllCst :: Typed a => [a] -> (Stream a -> Stream Bool) -> Stream Bool
forAllCst l f = conj $ map (f . constant) l

existsCst :: Typed a => [a] -> (Stream a -> Stream Bool) -> Stream Bool
existsCst l f = disj $ map (f . constant) l

allowed :: [Word64]
allowed = [1, 2]

majority :: Stream Word64 -> (Stream Word64, Stream Word64, Stream Bool)
majority l = (p, s, j)
  where
    p  = [0] ++ if s <= 0 then l else p
    s  = [0] ++ if (p == l) || (s <= 0) then s + 1 else s - 1

    k  = [0] ++ (1 + k)

    count m = cnt
      where
        cnt = [0] ++ if l == m then cnt + 1 else cnt

    j = forAllCst allowed $ \m ->
          local (count m) $ \cnt ->
          let j0 = (m /= p) ==> ((s + 2 * cnt) <= k)
              j1 = (m == p) ==> ((2 * cnt) <= (s + k))
          in j0 && j1

spec = do
  observer "i" input
  observer "p" p
  observer "s" s
  observer "j" j

  inRange <- prop "inRange" (forall $ input < 3)
  theorem "J"  (forall j) $ assume inRange >> induct

  where
    input = externW64 "in" (Just [1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1])
    (p, s, j) = majority input

induct :: Proof Universal
induct = induction def { nraNLSat = False, debug = False }

kinduct :: Word32 -> Proof Universal
kinduct k = kInduction def { nraNLSat = False, startK = k, maxK = k, debug = False }
