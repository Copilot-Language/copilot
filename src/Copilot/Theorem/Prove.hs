--------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns, ViewPatterns, ExistentialQuantification, GADTs #-}
{-# LANGUAGE Safe #-}

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
  ) where

import qualified Copilot.Core as Core

import Data.List (intercalate)
import Control.Applicative (liftA2)
import Control.Monad.Writer

--------------------------------------------------------------------------------

data Output = Output Status [String]

data Status = Sat | Valid | Invalid | Unknown | Error

{- Each prover has to provide the following five functions.
   The most important is `askProver`, which takes 3 arguments :
   *  The prover descriptor
   *  A list of properties names which are assumptions
   *  A property name which has to be deduced from these assumptions
-}

data Prover = forall r . Prover
  { proverName  :: String
  , startProver :: Core.Spec -> IO r
  , askProver   :: r -> [PropId] -> [PropId] -> IO Output
  , closeProver :: r -> IO ()
  }

type PropId = String

data PropRef a where
  PropRef :: PropId -> PropRef a

data Universal
data Existential

type Proof a = ProofScheme a ()

type UProof = Writer [Action] ()

data ProofScheme a b where
  Proof :: Writer [Action] b -> ProofScheme a b

instance Functor (ProofScheme a) where
  fmap = liftM

instance Applicative (ProofScheme a) where
  pure = return
  (<*>) = ap

instance Monad (ProofScheme a) where
  (Proof p) >>= f = Proof $ p >>= (\a -> case f a of Proof p -> p)
  return a = Proof (return a)

data Action where
  Check  :: Prover   -> Action
  Assume :: PropId   -> Action
  Admit  :: Action

--------------------------------------------------------------------------------

check :: Prover -> Proof a
check prover = Proof $ tell [Check prover]

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

        Assume propId -> do
          putStrLn $ propId ++ ": assumption"
          processActions (propId : context) nextActions

        Admit -> do
          putStrLn $ propId ++ ": admitted"
          processActions (propId : context) nextActions

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
