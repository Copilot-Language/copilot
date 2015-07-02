--------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

module Copilot.Kind.Prove
  ( prove
  ) where

import Copilot.Kind.Prover
import Copilot.Kind.ProofScheme

import qualified Copilot.Core as Core

import Control.Monad
import Control.Monad.Writer

import Data.List (intercalate)

--------------------------------------------------------------------------------

prove :: Prover -> ProofScheme -> Core.Spec -> IO ()
prove
  (Prover { proverName
          , hasFeature
          , startProver
          , askProver
          , closeProver } )
  (execWriter -> actions)
  spec = do

    prover <- startProver spec
    processActions prover [] actions
    putStrLn "Finished."
    closeProver prover

    where
      processActions _ _ [] = return ()
      processActions prover context (action:nextActions) = case action of
        Check propId -> do
          (Output status infos) <- askProver prover context [propId]
          case status of
            Valid     -> putStrLn $ propId ++ ": valid "
                                           ++ "(" ++ intercalate ", " infos ++ ")"
            Invalid   -> putStrLn $ propId ++ ": invalid "
                                           ++ "(" ++ intercalate ", " infos ++ ")"
            Error     -> putStrLn $ propId ++ ": error "
                                           ++ "(" ++ intercalate ", " infos ++ ")"
            Unknown   -> putStrLn $ propId ++ ": unknown "
                                           ++ "(" ++ intercalate ", " infos ++ ")"
          processActions prover context nextActions

        Assume propId -> do
          unless (hasFeature HandleAssumptions) $ do
            putStrLn $ "'" ++ proverName
                     ++ "' doesn't implement assumptions handling"
            processActions prover context nextActions

          processActions prover (propId : context) nextActions

        Pragma (PrintMsg s) -> do
          putStrLn s
          processActions prover context nextActions

        Local localActions -> do
          processActions prover context localActions
          processActions prover context nextActions

--------------------------------------------------------------------------------
