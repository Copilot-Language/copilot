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
  (dispatch, BackEnd(..), AtomToC(..), Interpreted(..), Iterations, Verbose(..)) where

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
    , interpreted :: Interpreted -- ^ Interpret the program or not
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

data BackEnd = Opts AtomToC 
             | Interpreter

data Interpreted = Interpreted | NotInterpreted
type Iterations = Int
data Verbose = OnlyErrors | DefaultVerbose | Verbose deriving Eq

-- | This function is the core of /Copilot/ :
-- it glues together analyser, interpreter and compiler, and does all the IO.
-- It can be called either from interface (which justs decodes the command-line argument)
-- or directly from the interactive prompt in ghci.
-- @streams@ is a specification, 
-- @inputExts@ allows the user to give at runtime values for
--      the monitored variables. Useful for testing on randomly generated values and specifications,
--      or for the interpreted version.
-- @be@ chooses between compilation or interpretation,
--      and if compilation is chosen (AtomToC) holds a few additionnal informations.
--      see description of @'BackEnd'@
-- @iterations@ just gives the number of periods the specification must be executed.
--      If you would rather execute it by hand, then just choose AtomToC for backEnd and 0 for iterations
-- @verbose@ determines what is output.
dispatch :: LangElems -> Vars -> BackEnd -> Iterations -> Verbose -> IO ()
dispatch elems inputExts backEnd iterations verbose =
    do
        hSetBuffering stdout LineBuffering
        mapM_ putStrLn preludeText 
        isValid <-
            case check (strms elems) of
                Just x -> print x >> return False
                Nothing -> return True
        when isValid $
            -- because haskell is lazy, will only get computed if later used
            let interpretedLines = showVars (interpretStreams (strms elems) trueInputExts) 
                                     iterations 
            in case backEnd of
                Interpreter -> 
                    do
                        unless allInputsPresents $ error errMsg
                        mapM_ putStrLn interpretedLines
                Opts opts ->
                    let isInterpreted =
                            case (interpreted opts) of
                                Interpreted -> True
                                NotInterpreted -> False
                        dirName = outputDir opts
                    in do
                        putStrLn $ "Trying to create the directory " ++ dirName 
                                     ++  " (if missing)  ..."
                        createDirectoryIfMissing False dirName
                        copilotToC elems allExts trueInputExts opts isVerbose
                        let copy ext = copyFile (cName opts ++ ext) 
                                        (dirName ++ cName opts ++ ext)
                        let delete ext = do 
                              f0 <- canonicalizePath (cName opts ++ ext) 
                              f1 <- canonicalizePath (dirName ++ cName opts ++ ext)
                              unless (f0 == f1) $ removeFile (cName opts ++ ext)
                        putStrLn $ "Moving " ++ cName opts ++ ".c and " ++ cName opts 
                                        ++ ".h to " ++ dirName ++ "  ..."
                        copy ".c"
                        copy ".h"
                        delete ".c"
                        delete ".h"
--                        when (prePostCode opts == Nothing) $ gccCall (Opts opts)
                        when (sim opts) (gccCall (Opts opts))
                        when ((isInterpreted || isExecuted) && not allInputsPresents) $ 
                            error errMsg
                        when isExecuted $ execute (strms elems) (dirName ++ cName opts) 
                                             trueInputExts isInterpreted 
                            interpretedLines iterations isSilent
    where
        errMsg = "The interpreter does not have values for some of the external variables."
        isVerbose = verbose == Verbose
        isSilent = verbose == OnlyErrors
        isExecuted = iterations /= 0
        allExts = getExternalVars (strms elems)
        (trueInputExts :: StreamableMaps [] , allInputsPresents :: Bool) = 
          filterStreamableMaps inputExts (map (\(a,v,r) -> (a, show v, r)) allExts)

copilotToC :: LangElems -> [Exs] -> Vars -> AtomToC -> Bool -> IO ()
copilotToC elems allExts trueInputExts opts isVerbose =
    let cFileName = cName opts
        (p', program) = copilotToAtom elems (getPeriod opts) cFileName
        (preCode, postCode) = 
            getPrePostCode (sim opts) (prePostCode opts) cFileName 
                           (strms elems) allExts (map (\(x,y) -> (ExtV x,y)) 
                           (arrDecs opts)) trueInputExts p'
            -- case (prePostCode opts) of
            --     Nothing ->  
            --       getPrePostCode cFileName (strms elems) allExts 
            --                      (map (\(x,y) -> (ExtV x,y)) (arrDecs opts))
            --                      trueInputExts p'
            --     Just (pre, post) -> (pre, post)
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
        when isVerbose (putStrLn $ A.reportSchedule sched)
        putStrLn $ "Generated " ++ cFileName ++ ".c and " ++ cFileName ++ ".h"

-- | Call Gcc to compile the code.
gccCall :: BackEnd -> IO ()
gccCall backend = 
  case backend of
    Interpreter -> error "Impossible: gccCall called with Interpreter."
    Opts opts   -> 
      let dirName = outputDir opts
          programName = cName opts
      in
        do
          let command = compiler opts ++ " " ++ dirName ++ cName opts 
                        ++ ".c" ++ " -o " ++ dirName ++ programName 
                        ++ " " ++ gccOpts opts
          putStrLn "Calling the C compiler  ..."
          putStrLn command
          _ <- system command
          return ()

execute :: StreamableMaps Spec -> String -> Vars -> Bool -> [String] -> Iterations -> Bool -> IO ()
execute streams programName trueInputExts isInterpreted interpretedLines iterations isSilent =
    do  (Just hin, Just hout, _, _) <- createProcess processConfig
        hSetBuffering hout LineBuffering
        hSetBuffering hin LineBuffering

        when isInterpreted 
                 (do putStrLn "\n *** Checking the randomly-generated Copilot specification: ***\n"
                     print streams)
        let inputVar v (val:vals) ioVars =
                do  hPutStr hin (showAsC val ++ " \n")
                    hFlush hin
                    vars <- ioVars
                    return $ updateSubMap (\m -> M.insert v vals m) vars
            inputVar _ [] _ = error "Impossible : empty inputVar"
            executePeriod _ [] 0 = putStrLn "Execution complete." 
            executePeriod _ [] _ = error "Impossible : empty ExecutePeriod"
            executePeriod inputExts (inLine:inLines) n =
                when (n > 0) $ 
                    do  nextInputExts <- foldStreamableMaps inputVar inputExts (return emptySM)
                        line <- hGetLine hout
                        let nextPeriod = 
                                (unless isSilent $ putStrLn line) >> 
                                    executePeriod nextInputExts inLines (n - 1)
                        -- Checking the compiler and interpreter.
                        if isInterpreted
                            then if inLine == line
                                       then nextPeriod
                                       else error $ unlines 
                                              [ "Failure: interpreter /= compiler"
                                              , "  program name: " ++ programName
                                              , "  interpreter: " ++ inLine
                                              , "  compiler: "    ++ line
                                              ]
                            else nextPeriod
                            
        -- useful for initializing the sampling temporary variables
        firstInputExts <- foldStreamableMaps inputVar trueInputExts (return emptySM)
        executePeriod firstInputExts interpretedLines iterations
    where
        processConfig = 
            (shell $ programName ++ " " ++ show iterations)
            {std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle stdout}

showVars :: Vars -> Int-> [String]
showVars interpretedVars n =
    showVarsLine interpretedVars 0
    where
        showVarsLine inVs i =
            if i == n 
                then []
                else 
                    let (string, inVs') = foldStreamableMaps prettyShow inVs ("", emptySM) 
                        endString = showVarsLine inVs' (i + 1) 
                        beginString = "period: " ++ show i ++ "   " ++ string 
                    in
                    beginString:endString
        prettyShow v l (s, vs) =
            let s' = v ++ ": " ++ showAsC (head l) ++ "   " ++ s
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
