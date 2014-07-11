--------------------------------------------------------------------------------

module Copilot.Kind.Prove 
  ( module Copilot.Kind.ProofScheme
  , prove 
  ) where

import Copilot.Kind.Prover
import Copilot.Kind.ProofScheme

import qualified Copilot.Core as Core

import Control.Monad

--------------------------------------------------------------------------------

prove :: Prover -> ProofScheme -> Core.Spec -> IO ()
prove 
  (Prover { proverName
          , hasFeature
          , startProver
          , askProver
          , closeProver } )
  (ProofScheme actions) 
  spec = do
  
    prover <- startProver spec
    processActions prover [] actions
    putStrLn "Finished."
    closeProver prover
  
    where 
      processActions _ _ [] = return ()
      processActions prover context (action:nextActions) = case action of
        Check propId -> do
          res <- askProver prover context [propId]
          case res of
            Valid     -> putStrLn $ propId ++ " : valid"
            Invalid _ -> putStrLn $ propId ++ " : invalid"
            Error s   -> putStrLn $ propId ++ " : error ++ (" ++ s ++ ")"
            Unknown   -> putStrLn $ propId ++ " : unknown"
          processActions prover context nextActions
          
        Assume propId -> do
          when (not $ hasFeature HandleAssumptions) $ do
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
      