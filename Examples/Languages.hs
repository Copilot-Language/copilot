-- | Examples of parsing various languages.  We'll assume input tokens come from
-- an external variable.  Assume the input doesn't given tokens outside the
-- alphabet, and the result is always delayed by one w.r.t. the input stream.

-- I think Copilot can express polynomial-time algorithms.

{-# LANGUAGE RebindableSyntax #-}

module Languages where

import Language.Copilot
import qualified Prelude as P
import qualified Data.List as L

---------------------------------------------------------------------------------
-- Regular expressions

{- 
We'll build a Copilot program to accept the regular language over the alphabet
{0,1} that contains an even number of 0s.  
-}

reAccept :: Spec
reAccept = do 
  observer "accept" accept
  observer "string" string
  where
  accept :: Stream Bool
  accept = [True] ++ if string == 0
                       then if accept then false
                              else true
                       else accept

  -- Input tokens.
  string :: Stream Word8
  string = [0] ++ if string == 0 then 1 else 0

-- interpret 10 reAccept

---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Context-free Grammars

{-
This Copilot program recognizes <0^n 1^n>, for n >= 0.  
-}

cfAccept :: Int -> Spec
cfAccept n = do
  observer "accept" accept
  observer "string" string
  where
  accept :: Stream Bool
  accept = if zerosSeen == 0 
             then true
             else false

  zerosSeen :: Stream Word64
  zerosSeen = [0] ++ if string == 0 
                       then zerosSeen + 1
                       else zerosSeen - 1

  -- Input tokens.
  string :: Stream Word8
  string = L.replicate n 0 P.++ L.replicate n 1 ++ 0 -- don't care about part of
                                                     -- stream after ++

-- interpret 40 (cfAccept 10)
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Context-sensitive grammars

{-
This Copilot program recognizes <0^n 1^n 2^n>, for n >= 0.  
-}

csAccept :: Int -> Spec
csAccept n = do
  observer "accept" accept
  observer "string" string
  where
  accept :: Stream Bool
  accept = if zerosSeen == 0 && onesSeen == 0
             then true
             else false

  zerosSeen :: Stream Word64
  zerosSeen = [0] ++ if string == 0
                           then zerosSeen + 1
                           else if string == 1 
                                  then zerosSeen - 1
                                  else zerosSeen

  onesSeen :: Stream Word64
  onesSeen = [0] ++ if string == 1
                      then onesSeen + 1
                      else if string == 0 
                             then onesSeen
                             else onesSeen - 1

  -- Input tokens.
  string :: Stream Word8
  string = L.replicate n 0 P.++ L.replicate n 1 P.++ L.replicate n 2 
             ++ 0 -- don't care about part of
                  -- stream after ++

-- interpret 40 (csAccept 5)
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Context-sensitive grammars

{-
I don't think the "copy language" <xx | x \in {0,1}*> can be recognized by a
Copilot program.  It requires non-constant space.  Note: some context-sensitive
grammars have PSACE recognizers.
-}

---------------------------------------------------------------------------------
-- -- Recognize the language of arbitrarily long sequence of prime numbers.

-- -- Sieve of Eratosthenes
-- primes :: Word64 -> [Word64]
-- primes n = primes' 2 nums

--   where 
--   nums = [2..n] 

--   f :: Word64 -> [Word64] -> [Word64]
--   f x = L.filter (\a -> P.not (P.rem a x P.== 0 P.&& a P.> x))

--   primes' :: Word64 -> [Word64] -> [Word64]
--   primes' x ls = let ls' = f x ls in
--                  -- Can't use rebinded if-the-else syntax
--                  case ls' P.== ls of
--                    True  -> ls
--                    False -> primes' (x P.+ 1) ls'

-- primesInf :: [Word64]
-- primesInf = foldr primes' [2] [3..]

--   where 

--   -- returns divisors that evenly divide x
--   f :: Word64 -> [Word64] -> Bool
--   f x ls = ls `seq` (L.or $ map (\a -> P.rem x a P.== 0) ls)
--      -- L.filter (\a -> P.rem x a P.== 0)

--   primes' :: Word64 -> [Word64] -> [Word64]
--   primes' next prms = case prms `seq` f next prms of
--                         True  -> prms `seq` (next:prms)
--                         False -> prms



-- primesAccept :: Word64 -> Spec
-- primesAccept n = do
--   observer "primes" primesStrm
--   observer "accept" accept

--   where
--   -- Assume we are implementing a Sieve of Eratosthenes
--   accept :: Stream Word64
--   accept = 

--   primesStrm :: Stream Word64
--   primesStrm = primes n ++ 0 -- don't care about rest of values after ++
