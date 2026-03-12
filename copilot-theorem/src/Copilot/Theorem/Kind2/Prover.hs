{-# LANGUAGE CPP  #-}
#if MIN_VERSION_base(4,19,0)
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif

-- | A prover backend based on Kind2.
module Copilot.Theorem.Kind2.Prover
  ( module Data.Default
  , Options (..)
  , kind2Prover
  ) where

import Copilot.Theorem.Prove
import Copilot.Theorem.Kind2.Output
import Copilot.Theorem.Kind2.PrettyPrint
import Copilot.Theorem.Kind2.Translate

-- It seems [IO.openTempFile] doesn't work on Mac OSX
import System.IO hiding (openTempFile)
import Copilot.Theorem.Misc.Utils (openTempFile)

import System.Process

import           System.Directory
import           Data.Default
import qualified Data.Map         as Map

import qualified Copilot.Theorem.Misc.Error as Err
import qualified Copilot.Theorem.TransSys   as TS

-- | Options for Kind2
data Options = Options
  { bmcMax :: Int -- ^ Upper bound on the number of unrolling that base and
                  --   step will perform. A value of 0 means /unlimited/.
  }

-- | Default options with unlimited unrolling for base and step.
instance Default Options where
  def = Options { bmcMax = 0 }

data ProverST = ProverST Options TS.TransSys

-- | A prover backend based on Kind2.
--
-- The executable @kind2@ must exist and its location be in the @PATH@.
kind2Prover :: Options -> Prover
kind2Prover opts = Prover
  { proverName =  "Kind2"
  , startProver  = return . ProverST opts . TS.translate
  , askProver    = askKind2
  , closeProver  = const $ return () }

kind2Prog :: String
kind2Prog = "kind2"

kind2BaseOptions :: [String]
kind2BaseOptions = ["--input-format", "native", "-xml"]

askKind2 :: ProverST -> [PropId] -> [PropId] -> IO Output
askKind2 _p _assumptions [] = fail "toCheck is empty"
askKind2 (ProverST opts spec) assumptions toCheck@(toCheckHead:_) = do

  let kind2Input = prettyPrint . toKind2 Inlined assumptions toCheck $ spec

  (tempName, tempHandle) <- openTempFile "." "out" "kind"
  hPutStr tempHandle kind2Input
  hClose tempHandle

  let kind2Options =
        kind2BaseOptions ++ ["--bmc_max", show $ bmcMax opts, tempName]

  (_, output, _) <- readProcessWithExitCode kind2Prog kind2Options ""

  putStrLn kind2Input

  removeFile tempName

  let propId         = toCheckHead
      propQuantifier = case Map.lookup propId (TS.specProps spec) of
                         Just (_, quantifier) ->
                           quantifier
                         Nothing ->
                           Err.impossible $
                             "askKind2: " ++ propId ++ " not in specProps"
  return $ parseOutput propId propQuantifier output
