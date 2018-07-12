module Language.Copilot.Main ( copilotMain, defaultMain ) where

import qualified Copilot.Core as C (Spec)
import Copilot.Language (interpret, prettyPrint)
import Copilot.Language.Reify (reify)
import Copilot.Language (Spec)

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)


type Interpreter  = Integer   ->   Spec -> IO ()
type Compiler     = FilePath  -> C.Spec -> IO ()
type Printer      =                Spec -> IO ()


data CmdArgs = CmdArgs
  { aoutput     :: String
  , acompile    :: Bool
  , apretty     :: Bool
  , ainterpret  :: Int
  }

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

defaultMain :: Compiler -> Spec -> IO ()
defaultMain = copilotMain interpret prettyPrint
