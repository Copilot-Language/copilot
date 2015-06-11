--------------------------------------------------------------------------------

module Copilot.Kind.Prover
  ( Cex     (..)
  , Output  (..)
  , Status  (..)
  , Feature (..)
  , Prover  (..)
  , combine
  ) where

import Copilot.Kind.ProofScheme
import qualified Copilot.Core as Core

import Data.List (intercalate)
import Control.Applicative (liftA2, liftA)

--------------------------------------------------------------------------------

data Cex = Cex

type Infos = [String]

data Output = Output Status Infos

data Status
  = Valid
  | Invalid (Maybe Cex)
  | Unknown
  | Error

data Feature = GiveCex | HandleAssumptions

{- Each prover has to provide the following five functions.
   The most important is `askProver`, which takes 3 arguments :
   *  The prover descriptor
   *  A list of properties names which are assumptions
   *  A property name which has to be deduced from these assumptions
-}

data Prover = forall r . Prover
  { proverName     :: String
  , hasFeature     :: Feature -> Bool
  , startProver    :: Core.Spec -> IO r
  , askProver      :: r -> [PropId] -> PropId -> IO Output
  , closeProver    :: r -> IO ()
  }

--------------------------------------------------------------------------------

combine :: Prover -> Prover -> Prover
combine
  (Prover { proverName  = proverNameL
          , hasFeature  = hasFeatureL
          , startProver = startProverL
          , askProver   = askProverL
          , closeProver = closeProverL
          })

  (Prover { proverName  = proverNameR
          , hasFeature  = hasFeatureR
          , startProver = startProverR
          , askProver   = askProverR
          , closeProver = closeProverR
          })

 = Prover
  { proverName  = proverNameL ++ "_" ++ proverNameR
  , hasFeature  = liftA2 (||) hasFeatureL hasFeatureR
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
    combineSt Error _                   = Error
    combineSt  _ Error                  = Error
    combineSt Valid Valid               = Valid
    combineSt Valid (Invalid _)         = Error
    combineSt Valid Unknown             = Valid
    combineSt (Invalid (Just cex)) _    = Invalid $ Just cex
    combineSt (Invalid _) (Invalid _)   = Invalid Nothing
    combineSt (Invalid Nothing) Unknown = Invalid Nothing
    combineSt Unknown Unknown           = Unknown
    combineSt o1 o2                     = combineSt o2 o1

    prefixMsg = case (stL, stR) of
      (Valid, Invalid _) -> ["The two provers don't agree"]
      _ -> []

    decoName s = "<" ++ s ++ ">"

    infos =
      prefixMsg
      ++ [decoName nameL]
      ++ msgL
      ++ [decoName nameR]
      ++ msgR

--------------------------------------------------------------------------------
