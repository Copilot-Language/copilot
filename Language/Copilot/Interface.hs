-- | Used by the end-user to easily give its arguments to dispatch.
module Language.Copilot.Interface (
          Options(), baseOpts, test, interpret, compile, verify, interface
        , help , setS, setE, setC, setO, setP, setI, setPP, setN, setV, setR
        , setDir, setGCC, setTriggers, setArrs,
        module Language.Copilot.Dispatch
    ) where

import Language.Copilot.Core
import Language.Copilot.Language (opsF, opsF2, opsF3)
import Language.Copilot.Tests.Random
import Language.Copilot.Dispatch
import Language.Copilot.Help

import Data.Set as Set (fromList, toList)
import Data.List ((\\))
import System.Random
import System.Exit
import System.Cmd
import Data.Maybe
import Control.Monad
import Control.Monad.Writer (execWriter)

data Options = Options {
        optStreams :: Maybe Streams, -- ^ If there's no Streams, then generate random streams.
        optSends :: Sends, -- ^ For distributed monitors.
        optExts :: Maybe Vars, -- ^ Assign values to external variables.
        optCompile :: Maybe String, -- ^ Set gcc options.
        optPeriod :: Maybe Period, -- ^ Set the period.  If none is given, then the smallest feasible period is computed.
        optInterpret :: Bool, -- ^ Interpreting?
        optIterations :: Int, -- ^ How many iterations are we simulating for?
        optVerbose :: Verbose, -- ^ Verbosity level: OnlyErrors | DefaultVerbose | Verbose.
        optRandomSeed :: Maybe Int, -- ^ Random seed for generating Copilot specs.
        optCName :: Name, -- ^ Name of the C file generated.
        optCompiler :: String, -- ^ The C compiler to use, as a path to the executable.
        optOutputDir :: String, -- ^ Where to place the output C files (.c, .h, and binary).
        optPrePostCode :: Maybe (String, String), -- ^ Code to append above and below the C file.
        optTriggers :: [(Var, String)], -- ^ A list of Copilot variable C
                                        -- function name pairs.  The C funciton
                                        -- is called if the Copilot stream
                                        -- becomes True.  The Stream must be of
                                        -- Booleans and the C function must be
                                        -- of type void @foo(void)@.  There
                                        -- should be no more than one function
                                        -- per trigger variable.  Triggers fire
                                        -- in the same phase (1) that output
                                        -- vars are assigned.
        optArrs :: [(String, Int)] -- ^ When generating C programs to test, we
                                   -- don't know how large external arrays are,
                                   -- so we cannot declare them.  Passing in
                                   -- pairs containing the name of the array and
                                   -- it's size allows them to be declared."
    }

baseOpts :: Options
baseOpts = Options {
        optStreams = Nothing,
        optSends = emptySM,
        optExts = Nothing,
        optCompile = Nothing,
        optPeriod = Nothing,
        optInterpret = False,
        optIterations = 0,
        optVerbose = DefaultVerbose,
        optRandomSeed = Nothing,
        optCName = "copilotProgram",
        optCompiler = "gcc",
        optOutputDir = "./",
        optPrePostCode = Nothing,
        optTriggers = [],
        optArrs = []
    }

-- Functions for making it easier for configuring copilot in the frequent use cases

test :: Int -> Options -> IO ()
test n opts = 
  interface $ setC "-Wall" $ setI $ setN n $ setV OnlyErrors $ opts 

interpret :: Streams -> Int -> Options -> IO ()
interpret streams n opts = interface $ setI $ setN n $ opts {optStreams = Just streams}

compile :: Streams -> Name -> Options -> IO ()
compile streams fileName opts = 
  interface $ setC "-Wall" $ setO fileName $ opts {optStreams = Just streams}

verify :: FilePath -> Int -> IO ()
verify file n = do
  putStrLn "Calling cbmc, developed by Daniel Kroening \& Edmund Clarke "
  putStrLn "<http://www.cprover.org/cbmc/>, with the following checks:"
  putStrLn "  --bounds-check       enable array bounds checks"
  putStrLn "  --div-by-zero-check  enable division by zero checks"
  putStrLn "  --pointer-check      enable pointer checks"
  putStrLn "  --overflow-check     enable arithmetic over- and underflow checks"
  putStrLn "  --nan-check          check floating-point for NaN"
  putStrLn ""
  putStrLn $ "  assuming main() as the entry point:"
  putStrLn cmd
  code <- system cmd
  case code of
    ExitSuccess -> return ()
    _           -> do putStrLn $ "cbmc could not be called on file " ++ file ++ "."
                      putStrLn "Perhaps cbmc is not installed on your system, is"
                      putStrLn "not in your path, cbmc cannot be called from the"
                      putStrLn $ "command line on your system, or " ++ file 
                      putStrLn "does not exist.  See <http://www.cprover.org/cbmc/>"
                      putStrLn "for more information on cbmc."
    where cmd = unwords ("cbmc" : args)
          args = ["--div-by-zero-check", "--overflow-check", "--bounds-check"
                 , "--nan-check", "--pointer-check"
                 , "--unwind " ++ show n, file] 
                   

-- Small functions for easy modification of the Options record

setS :: DistributedStreams -> Options -> Options
setS (streams, sends)  opts = opts {optStreams = Just streams, optSends = sends}

-- | Sets the environment for simulation by giving a mapping of external
-- variables to lists of values. E.g.,
-- 
-- @ setE (emptySM {w32Map = fromList [(\"ext\", [0,1..])]}) ... @
-- 
-- sets the external variable names "ext" to take the natural numbers, up to the limit of Word32.
setE :: Vars -> Options -> Options
setE vs opts = opts {optExts = Just vs}

setArrs :: [(String,Int)] -> Options -> Options
setArrs ls opts = opts {optArrs = ls}

-- | Sets the options for the compiler, e.g.,
--
-- @ setC \"-O2\" ... @
--
-- Sets gcc options.
setC :: String -> Options -> Options
setC gccOptions opts = opts {optCompile = Just gccOptions}

-- | Manually set the period for the program.  Otherwise, the minimum required period is computed automatically.  E.g.,
--
-- @ setP 100 ... @
-- sets the period to be 100.  The period must be at least 1.
setP :: Period -> Options -> Options
setP p opts = opts {optPeriod = Just p}

setI :: Options -> Options
setI opts = opts {optInterpret = True}

-- | Sets the number of iterations of the program to simulation:
--
-- @ setN 50 @
-- simulations the program for 50 steps.
setN :: Int -> Options -> Options
setN n opts = opts {optIterations = n}

-- | Set the verbosity level.  
setV :: Verbose -> Options -> Options
setV v opts = opts {optVerbose = v}

setR :: Int -> Options -> Options
setR seed opts = opts {optRandomSeed = Just seed}

setO :: Name -> Options -> Options
setO fileName opts = opts {optCName = fileName}

-- | Sets the compiler to use, given as a path to the executable.  The default
-- is \"gcc\".
setGCC :: String -> Options -> Options
setGCC compilerStr opts = opts {optCompiler = compilerStr}

-- | Sets the directory into which the output of compiled programs should be
-- placed.  If the directory does not exist, it will be created.
setDir :: String -> Options -> Options
setDir dir opts = opts {optOutputDir = dir}

-- | Sets the code to precede and follow the copilot specification
-- If nothing, then code appropriate for communication with the interpreter is generated
setPP :: (String, String) -> Options -> Options
setPP pp opts = opts {optPrePostCode = Just pp}

-- | Give C function triggers for Copilot Boolean streams.  The tiggers fire if
-- the stream becoms true.
setTriggers :: [(Var, String)] -> Options -> Options
setTriggers trigs opts = 
  if null repeats 
    then opts {optTriggers = trigs}
    else error $ "Error: only one trigger per Copilot variable.  Variables "
                 ++ show (Set.fromList repeats) ++ " are given multiple triggers."
  where vars = map fst trigs
        repeats = vars \\ Set.toList (Set.fromList vars)

-- | The "main" function
interface :: Options -> IO ()
interface opts =
    do
        seed <- createSeed opts 
        let (streams, vars) = getStreamsVars opts seed
            sends = optSends opts
            backEnd = getBackend opts seed
            iterations = optIterations opts
            verbose = optVerbose opts
        when (verbose == Verbose) $
            putStrLn $ "Random seed :" ++ show seed
        -- dispatch is doing all the heavy plumbing between 
        -- analyser, compiler, interpreter, gcc and the generated program
        dispatch streams sends vars backEnd iterations verbose 

createSeed :: Options -> IO Int
createSeed opts =
    case optRandomSeed opts of
        Nothing -> 
            do
                g <- newStdGen 
                return . fst . random $ g
        Just i -> return i

getStreamsVars :: Options -> Int -> (StreamableMaps Spec, Vars)
getStreamsVars opts seed = 
    case optStreams opts of
        Nothing -> randomStreams opsF opsF2 opsF3 (mkStdGen seed)
        Just s ->
            case optExts opts of
                Nothing -> (s', emptySM)
                Just vs -> (s', vs)
         where s' = execWriter s
        
getBackend :: Options -> Int -> BackEnd
getBackend opts seed =
    case optCompile opts of
        Nothing -> 
            if not $ optInterpret opts
                then error "neither interpreted nor compiled: nothing to be done"
                else Interpreter
        Just gccOptions ->
            Opts $ AtomToC {
                cName     = optCName opts ++ if isJust $ optRandomSeed opts then show seed else "",
                gccOpts   = gccOptions,
                getPeriod = optPeriod opts,
                interpreted = if optInterpret opts then Interpreted else NotInterpreted,
                outputDir = optOutputDir opts,
                compiler  = optCompiler opts,
                prePostCode = optPrePostCode opts,
                triggers = optTriggers opts,
                arrDecs = optArrs opts
                    }

help :: IO ()
help = putStrLn helpStr
