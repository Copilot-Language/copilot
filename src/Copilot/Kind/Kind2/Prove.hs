--------------------------------------------------------------------------------

module Copilot.Kind.Kind2.Prove where

import qualified Copilot.Core as Core
import Copilot.Kind.ProofScheme
import Copilot.Kind.Kind2.Output
import Copilot.Kind.Kind2.PrettyPrint
import Copilot.Kind.Kind2.Translate
import qualified Copilot.Kind.TransSys as TS


import Data.List (intercalate)
import System.Process
import System.IO
import Control.Monad (void)
import System.Directory

--------------------------------------------------------------------------------

kind2Prog    = "kind2"
kind2Options = ["--input-format", "native", "-xml"]

askKind2 :: TS.Spec -> [PropId] -> [PropId] -> IO (Maybe Bool)
askKind2 spec assumptions toCheck = do
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
  return res
  

prove :: ProofScheme -> Core.Spec -> IO ()
prove (ProofScheme actions) (TS.translate -> spec) = do

  processAction [] actions

  where 
    processAction _ [] = putStrLn "Finished."
    processAction context (action:nextActions) = case action of
      Check propId -> do
        res <- askKind2 spec context [propId]
        case res of
          Just True  -> putStrLn $ propId ++ " : valid"
          Just False -> putStrLn $ propId ++ " : invalid"
          Nothing    -> putStrLn $ propId ++ " : unknown"
        processAction (propId : context) nextActions
      
      Pragma (PrintMsg s) -> do
        putStrLn s
        processAction context nextActions
      
      _ -> putStrLn "Not handled yet"  

--------------------------------------------------------------------------------
      


