-- | Drives the checks between the interpreter and the compiler by starting the
-- compiled executable as a subprocess, sending values to it (over stdin),
-- marshalling values from it (stdout), and comparing them (as strings).

module Language.Copilot.Tests.Test
  (simCCode, Iterations, Verbose(..)) where

import Language.Copilot.Core
import Language.Copilot.PrettyPrinter ()

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Control.Monad (when, unless)
import System.Process 
import System.IO 
  (stderr, hSetBuffering, hClose, hGetLine, hPutStrLn, BufferMode(..), Handle)

type Iterations = Int
data Verbose = OnlyErrors | DefaultVerbose | Verbose deriving Eq

-- | Execute the generated C code using provided values for external
-- variables. (And possibly execute the interpreter, and compare the results, if
-- interpretedLines is defined .)
simCCode :: StreamableMaps Spec -> String -> SimValues -> Maybe [String] 
         -> Iterations -> Verbose -> IO ()
simCCode streams programName simExtValues mbInterpretedLines 
         iterations verbose = do
  (Just hin, Just hout, _, prc) <- createProcess processConfig

  when (verbose == Verbose) (putStrLn $ "\nTesting program:\n" 
                                          ++ unlines showStreams)
  -- Buffer by lines
  hSetBuffering hin LineBuffering

  -- Make the initial set of external variable values to send to the C program.
  inputVals <- foldStreamableMaps (inputVar hin) simExtValues (return emptySM)

  -- -1 Because we did an initial set of external vars.
  executePeriod (hin, hout, prc) inputVals mbInterpretedLines (iterations - 1) 

  hClose hout
  _ <- waitForProcess prc
  putStrLn "Execution complete." 

  where 

  -- Create a subprocess to execute the C program.  stdin and stdout for the C
  -- program, executed as a subprocess, will be redirected to hin and hout,
  -- respectively.  We'll keep stderr (for the C program) pointed at stderr.
  processConfig = 
    (shell $ programName ++ " " ++ show iterations) 
    {std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle stderr}

  -- inputVar is folded over the set of input values (as lists) given for
  -- external variables.  It returns a set of IO actions: for each external
  -- variable, the actions pipe the head of the list to the C simulator, then
  -- update the external variable map with the tail of the list.
  inputVar :: Streamable a 
           => Handle -> Var -> [a] -> IO SimValues -> IO SimValues
  inputVar _ _ [] _ = 
    error "Error: no more input values for external variables exist!"
  inputVar hin v (val:vals) ioSimVals = do
    _ <- hPutStrLn hin (showAsC val) 
    valMap <- ioSimVals
    return $ updateSubMap (\m -> M.insert v vals m) valMap

  -- Execute the C program for the specified number of periods, comparing the
  -- outputs to the results of the interpreter at each period, if we're in
  -- testing mode.
  -- 
  -- XXX The code below works, but don't change it unless you know what you're
  -- doing.  A less fragile version might use temporary files.
  executePeriod :: (Handle, Handle, ProcessHandle) -> SimValues -> Maybe [String] 
                -> Int -> IO ()
  executePeriod (hin, hout, _) _ mbInLines 0 = do
    hClose hin -- Important to close hin before getting hout.
    cLines <- hGetLine hout
    when (isJust mbInLines) 
         (compareOutputs ((concat . fromJust) mbInLines) cLines)
                         
  executePeriod ps@(hin, _, _) inputVals mbInLines n = do
    nextInputVals <- 
      foldStreamableMaps (inputVar hin) inputVals (return emptySM)
    let nextLines = case mbInLines of
                      Nothing -> Nothing
                      Just [] -> error "Impossible : empty ExecutePeriod"
                      Just xs -> Just xs
    executePeriod ps nextInputVals nextLines (n - 1)

  showStreams = 
    [ "****************************************************************\n"
    , show streams
    , "****************************************************************\n"
    ]

  -- Checking the compiler and interpreter.
  -- XXX This is pretty fragile, and depends on the string representations given
  -- by the interpreter and parser.  Ideally, this would be made more robust.
  compareOutputs :: String -> String -> IO ()
  compareOutputs inLine line = do
    unless (inLine == line) 
           -- Print to standard error, so you can push the testing to /dev/null.
           (hPutStrLn stderr failure >> error "Aborted testing on failure.")
    where 
    failure = unlines
       [ "\n*** Failed on the Copilot specification: ***\n"
       , unlines showStreams
       , ""
       , "Failure: interpreter /= compiler"
       , "  program name: " ++ programName
       , "  interpreter: " ++ inLine
       , "  compiler:    " ++ line
       ]
