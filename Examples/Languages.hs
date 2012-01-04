-- | Examples of parsing various languages.  We'll assume input tokens come from
-- an external variable.  Assume the input doesn't given tokens outside the
-- alphabet, and the result is always delayed by one w.r.t. the input stream.

{-# LANGUAGE RebindableSyntax #-}

module Languages where

import Language.Copilot
import qualified Prelude as P
import Data.List (replicate)

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
  string = replicate n 0 P.++ replicate n 1 ++ 0 -- don't care about part of
                                                 -- stream after ++

-- interpret 40 (cfAccept 10)
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Context-sensitive Grammars

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
  string = replicate n 0 P.++ replicate n 1 P.++ replicate n 2 
             ++ 0 -- don't care about part of
                  -- stream after ++

-- interpret 40 (csAccept 5)
---------------------------------------------------------------------------------
