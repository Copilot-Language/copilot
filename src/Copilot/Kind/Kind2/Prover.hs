--------------------------------------------------------------------------------

module Copilot.Kind.Kind2.Prover 
  ( module Copilot.Kind.ProofScheme
  , module Copilot.Kind.Prove
  , kind2  
  ) where

import Copilot.Kind.Prove
import qualified Copilot.Core as Core
import Copilot.Kind.ProofScheme
import Copilot.Kind.Kind2.Output
import Copilot.Kind.Kind2.PrettyPrint
import Copilot.Kind.Kind2.Translate
import qualified Copilot.Kind.TransSys as TS

import Copilot.Kind.Prover

import Data.List (intercalate)
import System.Process
import System.IO
import Control.Monad (void)
import System.Directory

--------------------------------------------------------------------------------

data ProverST = ProverST
  { transSys :: TS.Spec
  
  }

kind2 :: Prover
kind2 = Prover
  { proverName =  "Kind2"
  , hasFeature = \case
      GiveCex -> False
      HandleAssumptions -> False
  , startProver  = \spec -> return $ ProverST (TS.translate spec)
  , askProver    = askKind2
  , closeProver  = const $ return ()
  }
  
--------------------------------------------------------------------------------
  
kind2Prog    = "kind2"
kind2Options = ["--input-format", "native", "-xml"]

--------------------------------------------------------------------------------

askKind2 :: ProverST -> [PropId] -> [PropId] -> IO Output
askKind2 (ProverST spec) assumptions toCheck = do
  let kind2Input = prettyPrint . toKind2 Modular assumptions toCheck $ spec

  (tempName, tempHandle) <- openTempFile "." "out.kind"
  hPutStr tempHandle kind2Input
  hClose tempHandle
      
  (_, output, _) <- readProcessWithExitCode 
                      kind2Prog (kind2Options ++ [tempName]) ""          
                      
  -- putStrLn output
                 
  let res = fmap (all id) 
            . sequence 
            . map (flip isPropertyValid output) 
            $ toCheck
  
  removeFile tempName
  case res of
    Just True  -> return Valid
    Just False -> return (Invalid Nothing)
    Nothing    -> return $ Error "unexpected"
  
--------------------------------------------------------------------------------
      


