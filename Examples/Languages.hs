-- | Examples of parsing various languages.  We'll assume input tokens come from
-- an external variable.  Assume the input doesn't given tokens outside the
-- alphabet, and the result is always delayed by one w.r.t. the input stream.

-- Copilot can compute at least NP-Complete problems.

{-# LANGUAGE RebindableSyntax #-}

module Languages (languages) where

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
This Copilot program recognizes the "copy language" <xx | x \in {0,1}*>.

Note: the "trick" is to encode the history of streams in a bitvector.  Thus, we
can only recognize arbitrarily long words if we have arbitrarily long
bitvectors.  There is nothing in Copilot preventing this, but the largest base
type is currently a Word64.  

Without this encoding, we couldn't build a recognizers, because we can't
generate new streams on the fly or look back arbitrarily far in the history of a
stream; both are fixed at compile time.

-}


copyAccept :: Spec
copyAccept = do
  observer "accept" accept
  observer "hist" hist
  observer "string" string
  observer "cnt" cnt
  where

  accept :: Stream Bool
  accept = if cnt `mod` 2 == 1 then false else bottom == top 
    where
    halfCnt  = cnt `div` 2
    zeroBot  = (complement $ (2^halfCnt) - 1) .&. hist
    top      = zeroBot .>>. halfCnt
    bottom   = hist - zeroBot

  hist :: Stream Word64
  hist = [0] ++ ((2^cnt) * cast string) + hist

  cnt :: Stream Word64
  cnt = [0] ++ cnt + 1

  -- Input tokens.
  string :: Stream Word8
  string = let x = [1,0,0,1,0,1] in 
           x P.++ x
             ++ 0 -- don't care about part of
                  -- stream after ++

---------------------------------------------------------------------------------

languages :: IO ()
languages = do
  interpret 20 reAccept 
  interpret 20 (cfAccept 10)
  interpret 20 (csAccept 10)
  interpret 20 copyAccept

---------------------------------------------------------------------------------

main = languages

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
