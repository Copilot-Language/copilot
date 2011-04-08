{-# LANGUAGE ScopedTypeVariables #-}

-- | The Dispatch module : does all the IO, and offers an unified interface to both
-- the interpreter and the compiler.
--
-- Also communicates with GCC in order to compile the C code, and then transmits
-- the results of the execution of that C code.  This functionnality is mostly
-- used to check automatically the equivalence between the interpreter and the
-- compiler.  The Dispatch module only parses the command-line arguments before
-- calling that module.
module Language.Copilot.Dispatch 
  (dispatch, BackEnd(..), AtomToC(..), Iterations, Verbose(..)) where

import Debug.Trace
import Language.Copilot.Core
import Language.Copilot.Analyser (check, getExternalVars)
import Language.Copilot.Interpreter
import Language.Copilot.AtomToC
import Language.Copilot.Compiler
import Language.Copilot.PrettyPrinter ()

import qualified Language.Atom as A
import qualified Data.Map as M

import System.Directory 
import System.Process
import System.IO
import Control.Monad

data AtomToC = AtomToC 
    { cName :: Name -- ^ Name of the C file to generate
    , gccOpts :: String -- ^ Options to pass to the compiler
    , getPeriod :: Maybe Period -- ^ The optional period
--    , interpreted :: Interpreted -- ^ Interpret the program or not
    , outputDir :: String -- ^ Where to put the executable
    , compiler :: String -- ^ Which compiler to use
    , sim :: Bool -- ^ Are we running a C simulator?
    , prePostCode :: (Maybe String, Maybe String) -- ^ Code to replace the default
                                                  -- initialization and main
    , arrDecs :: [(String, Int)] -- ^ When generating C programs to test, we
                                 -- don't know how large external arrays are, so
                                 -- we cannot declare them.  Passing in pairs
                                 -- containing the name of the array and it's
                                 -- size allows them to be declared.
    , clock :: Maybe A.Clock     -- Use the hardware clock to drive the timing
                                 -- of the program.
    }

data BackEnd = Interpreter -- Just interpret
             | Compile AtomToC -- Just compile 
             | Test AtomToC -- Interpret and compile (and test, with random programs)

--data Interpreted = Interpreted | NotInterpreted
type Iterations = Int
data Verbose = OnlyErrors | DefaultVerbose | Verbose deriving Eq

-- | This function is the core of /Copilot/ :
-- it glues together analyser, interpreter and compiler, and does all the IO.
-- It can be called either from interface (which justs decodes the command-line argument)
-- or directly from the interactive prompt in ghci.
-- @elems@ is a specification (and possible triggers), 
-- @inputExts@ allows the user to give at runtime values for
--      the monitored variables. Useful for testing on randomly generated values and specifications,
--      or for the interpreted version.
-- @backend@ chooses between compilation or interpretation,
--      and if compilation is chosen (AtomToC) holds a few additionnal informations.
--      see description of @'BackEnd'@
-- @iterations@ just gives the number of periods the specification must be executed.
--      If you would rather execute it by hand, then just choose AtomToC for backEnd and 0 for iterations
-- @verbose@ determines what is output.
dispatch :: LangElems -> SimValues -> BackEnd -> Maybe Iterations -> Verbose -> IO ()
dispatch elems inputExts backEnd mIterations verbose = do
--  hSetBuffering stdout LineBuffering
  mapM_ putStrLn preludeText 
   -- run Copilot's typechecker/analyzer
  isValid <- case check (strms elems) of
               Just x -> print x >> return False
               Nothing -> return True
  when isValid $
    -- Ok, the Copilot program is valid.  What are we doing?n
    case backEnd of
      Interpreter -> do
        extVarValuesChks 
        mapM_ putStrLn interpretedLines

      Compile opts -> do 
        makeCFiles elems simExtValues allExts opts verbose
        when (sim opts) $ do extVarValuesChks 
                             gccCall opts
                             simC opts False
      Test opts -> do
        extVarValuesChks
        makeCFiles elems simExtValues allExts opts verbose
        when (sim opts) (gccCall opts)
        simC opts True

 where
   -- Do all the checks for feeding in external variable values.
   extVarValuesChks = do 
     unless allInputsPresent $ error missingExtVars
     unless (null inputsTooShort) $ error missingExtValues
   -- Simulate the generated C code, possibly checking it against the interpreter.
   simC opts b = 
     simCCode (strms elems) (outputDir opts ++ cName opts) simExtValues b
              interpretedLines iterations verbose
   iterations = case mIterations of
                  Nothing -> error "Internal copilot error: no iterations given"
                  Just i -> i
   -- Call the interpreter and pass the result to showVars, which prettyprints
   -- the results.
   interpretedLines = showVars (interpretStreams (strms elems) simExtValues) 
                        iterations 
   missingExtVars = 
     "The interpreter does not have values for some of the external variables."
   missingExtValues =
        "Error: the input values given for the external streams " 
     ++ show inputsTooShort ++ " must contain at least " ++ show iterations 
     ++ " (the number of iterations) elements."
   -- collect all the external variables from the Copilot program.
   allExts = getExternalVars (strms elems)
    -- check that all the external variables have streams given for them for
    -- simulation.
   (simExtValues :: SimValues , allInputsPresent :: Bool) = 
     filterStreamableMaps inputExts (map (\(a,v,r) -> (a, show v, r)) allExts)
   -- check that enough values are given for each external-variable
   -- value-stream.  (Can't just use length, since lists might be infinite.)
   inputsTooShort = 
     foldStreamableMaps (\v ls vs -> if length (take iterations ls) < iterations
                                       then v:vs else vs) 
       simExtValues [] 

makeCFiles :: LangElems -> SimValues -> [Exs] -> AtomToC -> Verbose -> IO ()
makeCFiles elems simExtValues allExts opts verbose = do
   let dirName = outputDir opts
   unless (last dirName == '/') 
          (error $ "Error: directory name for C files must contain a final '/'.  "
                   ++ "The directory name " ++ dirName ++ " does not.")
   putStrLn $ "Trying to create the directory " ++ dirName 
                ++  " (if missing)  ..."
   createDirectoryIfMissing False dirName
   copilotToC elems allExts simExtValues opts verbose
   let copy ext = copyFile (cName opts ++ ext) (dirName ++ cName opts ++ ext)
   let delete ext = do f0 <- canonicalizePath (cName opts ++ ext) 
                       f1 <- canonicalizePath (dirName ++ cName opts ++ ext)
                       unless (f0 == f1) $ removeFile (cName opts ++ ext)
   putStrLn $ "Moving " ++ cName opts ++ ".c and " ++ cName opts 
                 ++ ".h to " ++ dirName ++ "  ..."
   copy ".c"
   copy ".h"
   delete ".c"
   delete ".h"

copilotToC :: LangElems -> [Exs] -> SimValues -> AtomToC -> Verbose -> IO ()
copilotToC elems allExts simExtValues opts verbose =
    let cFileName = cName opts
        (p', program) = copilotToAtom elems (getPeriod opts) cFileName
        (preCode, postCode) = 
            getPrePostCode (sim opts) (prePostCode opts) cFileName 
                           (strms elems) allExts (map (\(x,y) -> (ExtV x,y)) 
                           (arrDecs opts)) simExtValues p'
        atomConfig = A.defaults 
            { A.cCode = \_ _ _ -> (preCode, postCode)
            , A.cRuleCoverage = False
            , A.cAssert = False
            , A.hardwareClock = clock opts
            , A.cStateName = "copilotState" ++ cFileName
            }
    in do
        putStrLn $ "Compiling Copilot specs to C  ..."
        (sched, _, _, _, _) <- A.compile cFileName atomConfig program
        when (verbose == Verbose) (putStrLn $ A.reportSchedule sched)
        putStrLn $ "Generated " ++ cFileName ++ ".c and " ++ cFileName ++ ".h"

-- | Call the C compiler.
gccCall :: AtomToC -> IO ()
gccCall opts = 
  let dirName = outputDir opts
      programName = cName opts 
      command = compiler opts ++ " " ++ dirName ++ cName opts 
                  ++ ".c" ++ " -o " ++ dirName ++ programName 
                  ++ " " ++ gccOpts opts
  in do putStrLn "Calling the C compiler  ..."
        putStrLn command
        _ <- system command
        return ()

-- | Execute the generated C code, interpreter, and compare the results.
simCCode :: StreamableMaps Spec -> String -> SimValues -> Bool -> [String] 
         -> Iterations -> Verbose -> IO ()
simCCode streams programName simExtValues isInterpreted interpretedLines 
         iterations verbose = do
  (Just hin, Just hout, _, proc) <- createProcess processConfig
  hSetBuffering hout LineBuffering
  hSetBuffering hin LineBuffering
  when isInterpreted 
       (do putStrLn "\n *** Checking the randomly-generated Copilot specification: ***\n"
           print streams)
  let inputVar v (val:vals) ioVars = do
        hPutStr hin (showAsC val ++ " \n")
        hFlush hin
        vars <- ioVars
        return $ updateSubMap (\m -> M.insert v vals m) vars
      inputVar _ [] _ = error "Impossible : empty inputVar"
      executePeriod _ [] 0 = do putStrLn "Execution complete." 
                                _ <- waitForProcess proc
                                return ()
      executePeriod _ [] _ = error "Impossible : empty ExecutePeriod"
      executePeriod inputExts (inLine:inLines) n = do
        nextInputExts <- foldStreamableMaps inputVar inputExts (return emptySM)
        line <- hGetLine hout
        let nextPeriod = (unless (verbose == OnlyErrors) $ 
                            putStrLn line) 
                         >> executePeriod nextInputExts inLines (n - 1)
        if isInterpreted 
          then compareOutputs inLine line nextPeriod programName
          else nextPeriod
  -- useful for initializing the sampling temporary variables
  firstInputExts <- foldStreamableMaps inputVar simExtValues (return emptySM)
  executePeriod firstInputExts interpretedLines iterations
  where processConfig = 
          (shell $ programName ++ " " ++ show iterations)
          {std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle stdout}

-- Checking the compiler and interpreter.
compareOutputs :: String -> String -> IO () -> String -> IO ()
compareOutputs inLine line nextPeriod programName =
  if inLine == line
    then nextPeriod
    else error $ unlines 
             [ "Failure: interpreter /= compiler"
             , "  program name: " ++ programName
             , "  interpreter: " ++ inLine
             , "  compiler: "    ++ line
             ]


-- | Prettyprint the interpreted values.
showVars :: SimValues -> Int-> [String]
showVars interpretedVars n =
    showVarsLine interpretedVars 0
    where
        showVarsLine copilotVs i =
            if i == n 
                then []
                else 
                    let (string, copilotVs') = foldStreamableMaps prettyShow copilotVs ("", emptySM) 
                        endString = showVarsLine copilotVs' (i + 1) 
                        beginString = "period: " ++ show i ++ "   " ++ string 
                    in  beginString:endString
        prettyShow v l (s, vs) = 
            let s' = v ++ ": " ++ showAsC head' ++ "   " ++ s
                head' = if null l then error "Copilot: internal error in the interpreter." else head l
                vs' = updateSubMap (\ m -> M.insert v (tail l) m) vs in
            (s', vs') 
                
preludeText :: [String]
preludeText = 
    [ ""
    , "========================================================================="
    , "  CoPilot, a stream language for generating hard real-time C monitors.  "
    , "========================================================================="
    , "Copyright, Galois, Inc. 2010"
    , "BSD3 License" 
    , "Website: http://leepike.github.com/Copilot/"
    , "Maintainer: Lee Pike <leepike--at--gmail.com> (remove dashes)."
    , "Usage: > help"
    , "========================================================================="
    , ""
    ]
