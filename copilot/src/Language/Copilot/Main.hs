-- | Create Copilot executables that generate code or interpret streams and
-- print the results to stdout.
module Language.Copilot.Main ( copilotMain, defaultMain ) where

import qualified Copilot.Core as C (Spec)
import Copilot.Language (interpret, prettyPrint)
import Copilot.Language.Reify (reify)
import Copilot.Language (Spec)

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)

-- | An interpreter of Copilot specifications for a given
-- number of simulation steps.
type Interpreter  = Integer   ->   Spec -> IO ()

-- | A compiler from
-- <https://hackage.haskell.org/package/copilot-core Copilot Core>
-- specifications.
type Compiler     = FilePath  -> C.Spec -> IO ()

-- | A pretty printer of Copilot specifications.
type Printer      =                Spec -> IO ()

-- | Command line arguments supported by all commands in 'cmdargs'.
data CmdArgs = CmdArgs
  { aoutput     :: String
  , acompile    :: Bool
  , apretty     :: Bool
  , ainterpret  :: Int
  }

-- | Command line arguments handled by the Copilot main function.
cmdargs :: Parser CmdArgs
cmdargs = CmdArgs
  <$> strOption (long "output"  <> short 'o' <> value "."
                                <> help "Output directory of C files")
  <*> switch  (long "justrun" <> short 'c'
                              <> help "Do NOT produce *.c and *.h files as output")
  <*> switch  (long "print" <> short 'p'
                            <> help "Pretty print the specification")
  <*> option auto (long "interpret" <> short 'i' <> value 0
                                    <> metavar "INT" <> showDefault
                                    <> help "Interpret specification and write result to output")

-- | Create a main to either compile or interpret a copilot specification.
--
-- This function must be provided an auxiliary function capable of compiling
-- <https://hackage.haskell.org/package/copilot-core Copilot Core>
-- specifications for some target.
--
-- The command line program supports four main commands:
--
--     * @--output/-o@: use the given compiler to produce C code.
--
--     * @--justrun/-c@: execute a dry-run, which parses and converts the
--       specification to core but does not produce any output.
--
--     * @--print/-p@: pretty print the specification.
--
--     * @--interpret/-i NUM@: interpret the specification for a given number
--       of steps.
copilotMain :: Interpreter -> Printer -> Compiler -> Spec -> IO ()
copilotMain interp pretty comp spec = main =<< execParser opts where
  opts = info (cmdargs <**> helper) fullDesc

  main :: CmdArgs -> IO ()
  main args = do
    let iters = ainterpret args
    when (apretty args)       $ pretty spec
    when (iters Prelude.> 0)  $ interp (fromIntegral iters) spec

    when (not $ acompile args) $ do
      spec' <- reify spec
      comp (aoutput args) spec'

-- | Create a main function with a default interpreter and pretty printer.
--
-- This function must be provided an auxiliary function capable of compiling
-- <https://hackage.haskell.org/package/copilot-core Copilot Core>
-- specifications for some target.
--
-- This function relies on 'copilotMain', please refer to that function for the
-- command line options.
defaultMain :: Compiler -> Spec -> IO ()
defaultMain = copilotMain interpret prettyPrint
