--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE Safe                      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Connection to theorem provers.
module Copilot.Theorem.Prove
  ( Output  (..)
  , Status  (..)
  , Prover  (..)
  , PropId, PropRef (..)
  , Proof, UProof, ProofScheme (..)
  , Action (..)
  , Universal, Existential
  , check
  , prove
  , combine
  ) where

import qualified Copilot.Core as Core

import Data.List (intercalate)
import Control.Applicative (liftA2)
import Control.Monad.Writer

--------------------------------------------------------------------------------

-- | Output produced by a prover, containing the 'Status' of the proof and
-- additional information.
data Output = Output Status [String]

-- | Status returned by a prover when given a specification and a property to
-- prove.
data Status = Sat | Valid | Invalid | Unknown | Error

-- | A connection to a prover able to check the satisfiability of
-- specifications.
--
-- The most important is `askProver`, which takes 3 arguments :
--
-- *  The prover descriptor
--
-- *  A list of properties names which are assumptions
--
-- *  A properties that have to be deduced from these assumptions
data Prover = forall r . Prover
  { proverName  :: String
  , startProver :: Core.Spec -> IO r
  , askProver   :: r -> [PropId] -> [PropId] -> IO Output
  , closeProver :: r -> IO ()
  }

-- | A unique property identifier
type PropId = String

-- | Reference to a property.
data PropRef a where
  PropRef :: PropId -> PropRef a

-- | Empty datatype to mark proofs of universally quantified predicates.
data Universal

-- | Empty datatype to mark proofs of existentially quantified predicates.
data Existential

-- | A proof scheme with unit result.
type Proof a = ProofScheme a ()

-- | A sequence of computations that generate a trace of required prover
-- 'Action's.
type UProof = Writer [Action] ()

-- | A proof scheme is a sequence of computations that compute a result and
-- generate a trace of required prover 'Action's.
data ProofScheme a b where
  Proof :: Writer [Action] b -> ProofScheme a b

instance Functor (ProofScheme a) where
  fmap = liftM

instance Applicative (ProofScheme a) where
  pure = return
  (<*>) = ap

instance Monad (ProofScheme a) where
  (Proof p) >>= f = Proof $ p >>= (\a -> case f a of Proof p' -> p')
  return a = Proof (return a)

-- | Prover actions.
data Action where
  Check  :: Prover -> Action
  Assume :: PropId -> Action
  Admit  :: Action

--------------------------------------------------------------------------------

-- | Record a requirement for satisfiability checking.
check :: Prover -> Proof a
check prover = Proof $ tell [Check prover]

-- | Try to prove a property in a specification with a given proof scheme.
--
-- Return 'True' if a proof of satisfiability or validity is found, false
-- otherwise.
prove :: Core.Spec -> PropId -> UProof -> IO Bool
prove spec propId (execWriter -> actions) = do

    thms <- processActions [] actions
    putStr $ "Finished: " ++ propId ++ ": proof "
    if (elem propId thms) then (putStrLn "checked successfully") else (putStrLn "failed")
    return $ elem propId thms

    where
      processActions context [] = return context
      processActions context (action:nextActions) = case action of
        Check (Prover { startProver, askProver, closeProver }) -> do
          prover <- startProver spec
          (Output status infos) <- askProver prover context [propId]
          closeProver prover
          case status of
            Sat     -> do
              putStrLn $ propId ++ ": sat " ++ "(" ++ intercalate ", " infos ++ ")"
              processActions (propId : context) nextActions
            Valid   -> do
              putStrLn $ propId ++ ": valid " ++ "(" ++ intercalate ", " infos ++ ")"
              processActions (propId : context) nextActions
            Invalid -> do
              putStrLn $ propId ++ ": invalid " ++ "(" ++ intercalate ", " infos ++ ")"
              processActions context nextActions
            Error   -> do
              putStrLn $ propId ++ ": error " ++ "(" ++ intercalate ", " infos ++ ")"
              processActions context nextActions
            Unknown -> do
              putStrLn $ propId ++ ": unknown " ++ "(" ++ intercalate ", " infos ++ ")"
              processActions context nextActions

        Assume propId' -> do
          putStrLn $ propId' ++ ": assumption"
          processActions (propId' : context) nextActions

        Admit -> do
          putStrLn $ propId ++ ": admitted"
          processActions (propId : context) nextActions

-- | Combine two provers producing a new prover that will run both provers in
-- parallel and combine their outputs.
--
-- The results produced by the provers must be consistent. For example, if one
-- of the provers indicates that a property is 'Valid' while another indicates
-- that it is 'Invalid', the combination of both provers will return an
-- 'Error'.
combine :: Prover -> Prover -> Prover
combine
  (Prover { proverName  = proverNameL
          , startProver = startProverL
          , askProver   = askProverL
          , closeProver = closeProverL
          })

  (Prover { proverName  = proverNameR
          , startProver = startProverR
          , askProver   = askProverR
          , closeProver = closeProverR
          })

 = Prover
  { proverName  = proverNameL ++ "_" ++ proverNameR
  , startProver = \spec -> do
      proverL <- startProverL spec
      proverR <- startProverR spec
      return (proverL, proverR)

  , askProver = \(stL, stR) assumptions toCheck ->
      liftA2 (combineOutputs proverNameL proverNameR)
        (askProverL stL assumptions toCheck)
        (askProverR stR assumptions toCheck)

  , closeProver = \(stL, stR) -> do
      closeProverL stL
      closeProverR stR
  }

combineOutputs :: [Char] -> [Char] -> Output -> Output -> Output
combineOutputs nameL nameR (Output stL msgL) (Output stR msgR) =
  Output (combineSt stL stR) infos

  where
    combineSt Error _         = Error
    combineSt  _ Error        = Error

    combineSt Valid Invalid   = Error
    combineSt Invalid Valid   = Error

    combineSt Invalid _       = Invalid
    combineSt _ Invalid       = Invalid

    combineSt Valid _         = Valid
    combineSt _ Valid         = Valid

    combineSt Sat _           = Sat
    combineSt _ Sat           = Sat

    combineSt Unknown Unknown = Unknown

    prefixMsg = case (stL, stR) of
      (Valid, Invalid) -> ["The two provers don't agree"]
      _ -> []

    decoName s = "<" ++ s ++ ">"

    infos =
      prefixMsg
      ++ [decoName nameL]
      ++ msgL
      ++ [decoName nameR]
      ++ msgR

--------------------------------------------------------------------------------
