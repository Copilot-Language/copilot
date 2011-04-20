{-# LANGUAGE FlexibleInstances #-}

-- | Used by the end-user to easily give its arguments to dispatch.
module Language.Copilot.Interface (
    Options(), baseOpts, randomTest, randomTests, test, interpret, compile
  , verify, interface, help, setC, setO, setP, setI, setCode --, setN
  , setV, setR, setDir, setGCC, setArrs, setClock, setSim, Verbose(..)
  -- * For setting the environments
  , setE, setEB, setEW8, setEW16, setEW32, setEW64
  , setEI8, setEI16, setEI32, setEI64, setEF, setED
  ) where

import Language.Copilot.Core
import Language.Copilot.Language.RandomOps (opsF, opsF2, opsF3)
import Language.Copilot.Tests.Random
import Language.Copilot.Dispatch 
import Language.Copilot.Help
import qualified Language.Atom as A (Clock)

import Data.Map (insert)
import Data.Word
import Data.Int
import System.Random
import System.Exit
import System.Cmd
import Data.Maybe
import qualified Data.Map as M (empty)
--import System.Mem (performGC)

data Options = Options {
  optStreams :: Maybe (StreamableMaps Spec), -- ^ If there's no Streams, then
                                             -- generate random streams.
  optSimVals :: Maybe SimValues, -- ^ Assign values to external variables.
  optCompile :: String, -- ^ Set gcc options.
  optPeriod :: Maybe Period, -- ^ Set the period.  If none is given, then the
                             -- smallest feasible period is computed.
  optInterpret :: Bool, -- ^ Interpreting?
  optIterations :: Maybe Int, -- ^ How many iterations are we simulating for?
  optVerbose :: Verbose, -- ^ Verbosity level: OnlyErrors | DefaultVerbose |
                         -- Verbose.
  optRandomSeed :: Maybe Int, -- ^ Random seed for generating Copilot specs.
  optCName :: Name, -- ^ Name of the C file generated.
  optCompiler :: String, -- ^ The C compiler to use, as a path to the
                         -- executable.
  optOutputDir :: String, -- ^ Where to place the output C files (.c, .h, and
                          -- binary).
  optPrePostCode :: (Maybe String, Maybe String), -- ^ Code to append above and
                                                  -- below the C file.
  optSimulate :: Bool, -- ^ Do we want a simulation driver?
  optTriggers :: Triggers, -- ^ A list of Copilot variable C function name
                           -- pairs.  The C funciton is called if the Copilot
                           -- stream becomes True.  The Stream must be of
                           -- Booleans and the C function must be of type void
                           -- @foo(void)@.  There should be no more than one
                           -- function per trigger variable.  Triggers fire in
                           -- the same phase (1) that output vars are assigned.
  optArrs :: [(String, Int)], -- ^ When generating C programs to test, we don't
                              -- know how large external arrays are, so we
                              -- cannot declare them.  Passing in pairs
                              -- containing the name of the array and it's size
                              -- allows them to be declared.
  optClock :: Maybe A.Clock  -- ^ Use the hardware clock to drive the timing of
                             -- the program?
    }

baseOpts :: Options
baseOpts = Options {
        optStreams = Nothing,
        optSimVals = Nothing,
        optCompile = "",
        optPeriod = Nothing,
        optInterpret = False,
        optIterations = Nothing,
        optVerbose = DefaultVerbose,
        optRandomSeed = Nothing,
        optCName = "copilotProgram",
        optCompiler = "gcc",
        optOutputDir = "./",
        optPrePostCode = (Nothing, Nothing),
        optSimulate = False,
        optTriggers = M.empty,
        optArrs = [],
        optClock = Nothing
    }

-- Functions for making it easier for configuring copilot in the frequent use
-- cases

-- | Run 'randomTest' @i@ iterations on each of @n@ random programs.
randomTests :: Int -> Int -> Options -> IO ()
randomTests n i opts = 
  sequence_ $ replicate i (randomTest n opts)

-- | Generate a random Copilot program and compare the interpreter against the
-- compiler on that program.
randomTest :: Int -> Options -> IO ()
randomTest n opts = do
  r <- randomRIO (0, maxBound)
  interface $ setC "-Wall" $ setI $ setR r $ setSim n opts
    
-- | Compare the interpreter and the compiler on a specific program.
test :: Streams -> Name -> Int -> Options -> IO ()
test streams fileName n opts =
  interface $ setC "-Wall" $ setO fileName $ setSim n
    $ setTriggers (getTriggers streams) 
      $ setI $ opts {optStreams = Just (getSpecs streams)}

-- | Interpret a program.
interpret :: Streams -> Int -> Options -> IO ()
interpret streams n opts =
  interface $ setI $ setN n $ opts {optStreams = Just (getSpecs streams)}

-- | Compile a program.
compile :: Streams -> Name -> Options -> IO ()
compile streams fileName opts = 
  interface $ setC "-Wall" $ setO fileName 
    $ setTriggers (getTriggers streams) 
      $ opts {optStreams = Just (getSpecs streams)}

-- | Verify the output of a compiled program using @CBMC@.
verify :: FilePath -> Int -> String -> IO ()
verify file n opts = do
  putStrLn "Calling cbmc, developed by Daniel Kroening \& Edmund Clarke."
  putStrLn "<http://www.cprover.org/cbmc/>, with the following checks:"
  putStrLn "  --bounds-check       enable array bounds checks"
  putStrLn "  --div-by-zero-check  enable division by zero checks"
  putStrLn "  --pointer-check      enable pointer checks"
  putStrLn "  --overflow-check     enable arithmetic over- and underflow checks"
  putStrLn "  --nan-check          check floating-point for NaN"
  putStrLn ""
  putStrLn $ "Assuming main() as the entry point unless specified otherwise " 
             ++ "(--function f)"
  putStrLn cmd
  code <- system cmd
  case code of
    ExitSuccess -> return ()
    _           -> do putStrLn ""
                      putStrLn $ "An error was returned by cbmc.  This may have " 
                                 ++ "be due to cbmc not finding the file " 
                                 ++ file ++ ".  Perhaps cbmc is not installed on"
                                 ++ " your system, is not in your path, cbmc"
                                 ++ " cannot be called from the command line"
                                 ++ " on your system, or " ++ file 
                                 ++ " does not exist.  See "
                                 ++ "<http://www.cprover.org/cbmc/> "
                                 ++ "for more information on cbmc."
    where cmd = unwords ("cbmc" : args)
          args = ["--div-by-zero-check", "--overflow-check", "--bounds-check"
                 , "--nan-check", "--pointer-check"
                 , "--unwind " ++ show n, opts, file]


-- Small functions for easy modification of the Options record

-- -- | Set the directives for sending stream values on ports.
setTriggers :: Triggers -> Options -> Options
setTriggers triggers opts = opts {optTriggers = triggers}

setE :: Streamable a => Var -> [a] -> Options -> Options
setE v ls opts = 
  opts {optSimVals = Just $ updateSubMap (\m -> insert v ls m) mp}
  where mp = if isNothing (optSimVals opts) then emptySM
               else fromJust $ optSimVals opts

setEB :: Var -> [Bool] -> Options -> Options
setEB = setE
setEI8 :: Var -> [Int8] -> Options -> Options
setEI8 = setE
setEI16 :: Var -> [Int16] -> Options -> Options
setEI16 = setE
setEI32 :: Var -> [Int32] -> Options -> Options
setEI32 = setE
setEI64 :: Var -> [Int64] -> Options -> Options
setEI64 = setE
setEW8 :: Var -> [Word8] -> Options -> Options
setEW8 = setE 
setEW16 :: Var -> [Word16] -> Options -> Options
setEW16 = setE
setEW32 :: Var -> [Word32] -> Options -> Options
setEW32 = setE
setEW64 :: Var -> [Word64] -> Options -> Options
setEW64 = setE
setEF :: Var -> [Float] -> Options -> Options
setEF = setE
setED :: Var -> [Double] -> Options -> Options
setED = setE


-- | Sets the environment for simulation by giving a mapping of external
-- variables to lists of values. E.g.,
--
-- @ setE (emptySM {w32Map = fromList [(\"ext\", [0,1..])]}) ... @
--
-- sets the external variable names "ext" to take the natural numbers, up to the
-- limit of Word32.
-- setE :: SimValues -> Options -> Options
-- setE vs opts = opts {optSimVals = Just vs}

-- | Set the external arrays.
setArrs :: [(String,Int)] -> Options -> Options
setArrs ls opts = opts {optArrs = ls}

-- | Sets the hardware clock.  See
-- http://github.com/tomahawkins/atom/blob/master/Language/Atom/Code.hs.
setClock :: A.Clock -> Options -> Options
setClock clk opts = opts {optClock = Just clk}

-- | Sets the options for the compiler, e.g.,
--
-- @ setC \"-O2\" ... @
--
-- Sets gcc options.
setC :: String -> Options -> Options
setC gccOptions opts = opts {optCompile = gccOptions}

-- | Manually set the period for the program.  Otherwise, the minimum required
-- period is computed automatically.  E.g.,
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
setN n opts = opts {optIterations = Just n}

-- | Set the verbosity level.
setV :: Verbose -> Options -> Options
setV v opts = opts {optVerbose = v}

-- | Set the random seed for generating random `Copilot` specs.
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
setCode :: (Maybe String, Maybe String) -> Options -> Options
setCode pp opts = opts {optPrePostCode = pp}

-- | Include simulation driver code (in a main() loop)?  If so, how many times
-- should we simulate it/
setSim :: Int -> Options -> Options
setSim n opts = opts {optIterations = Just n, optSimulate = True}

-- | The "main" function that dispatches.
interface :: Options -> IO ()
interface opts =
    do  seed <- createSeed opts -- called lazily
        let (streams, vars) = getStreamsVars opts seed
            triggers = optTriggers opts
            backEnd = getBackend opts seed
            iterations = optIterations opts
            verbose = optVerbose opts
        -- when (verbose == Verbose) $
        --     putStrLn $ "Random seed :" ++ show seed
        -- dispatch is doing all the heavy plumbing between
        -- analyser, compiler, interpreter, gcc and the generated program
        dispatch (LangElems streams triggers) vars backEnd iterations verbose 

createSeed :: Options -> IO Int
createSeed opts =
    case optRandomSeed opts of
        Nothing ->
            do
                g <- newStdGen
                return . fst . random $ g
        Just i -> return i

-- | Were streams given?  If not, just make random streams.
getStreamsVars :: Options -> Int -> (StreamableMaps Spec, SimValues)
getStreamsVars opts seed =
    case optStreams opts of
        Nothing -> randomStreams opsF opsF2 opsF3 (mkStdGen seed)
        Just s ->
            case optSimVals opts of
                Nothing -> (s, emptySM)
                Just vs -> (s, vs)

-- | Are we interpreting, compiling, or comparing the interpreter to the compiler?
getBackend :: Options -> Int -> BackEnd
getBackend opts seed =
  if optInterpret opts
    then if optSimulate opts
           then Test backendOpts
           else Interpreter
    else Compile backendOpts
  where 
    backendOpts =
        AtomToC { cName = 
                  optCName opts ++ if isJust $ optRandomSeed opts 
                                     then show seed else ""
                , randomProg = isJust $ optRandomSeed opts 
                , gccOpts     =  optCompile opts
                , getPeriod   = optPeriod opts
                , outputDir   = optOutputDir opts
                , compiler    = optCompiler opts
                , prePostCode = optPrePostCode opts
                , sim     = optSimulate opts
                , arrDecs = optArrs opts
                , clock   = optClock opts
                }

help :: IO ()
help = putStrLn helpStr
