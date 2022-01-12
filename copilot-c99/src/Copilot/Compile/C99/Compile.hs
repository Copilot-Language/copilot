-- | Compile Copilot specifications to C99 code.
module Copilot.Compile.C99.Compile
  ( compile
  , compileWith
  ) where

import Text.PrettyPrint     (render)
import Data.List            (nub)
import Data.Maybe           (catMaybes)
import System.Directory     (createDirectoryIfMissing)
import System.Exit          (exitFailure)
import System.FilePath      ((</>))
import System.IO            (hPutStrLn, stderr)

import Language.C99.Pretty  (pretty)
import qualified Language.C99.Simple as C

import Copilot.Core
import Copilot.Compile.C99.Util
import Copilot.Compile.C99.External
import Copilot.Compile.C99.Settings
import Copilot.Compile.C99.Translate
import Copilot.Compile.C99.CodeGen

-- | Compile a specification to a .h and a .c file.
--
-- The first argument is the settings for the C code generated.
--
-- The second argument is used as prefix for the .h and .c files generated.
compileWith :: CSettings -> String -> Spec -> IO ()
compileWith cSettings prefix spec
  | null (specTriggers spec)
  = do hPutStrLn stderr $
         "Copilot error: attempt at compiling empty specification.\n"
         ++ "You must define at least one trigger to generate C monitors."
       exitFailure

  | otherwise
  = do let cfile = render $ pretty $ C.translate $ compilec cSettings spec
           hfile = render $ pretty $ C.translate $ compileh cSettings spec

           -- TODO: find a nicer solution using annotated AST's
           -- Should figure out exactly which headers are needed, based on what
           -- is used.
           cmacros = unlines [ "#include <stdint.h>"
                             , "#include <stdbool.h>"
                             , "#include <string.h>"
                             , "#include <stdlib.h>"
                             , "#include <math.h>"
                             , ""
                             , "#include \"" ++ prefix ++ ".h\""
                             , ""
                             ]

       let dir = cSettingsOutputDirectory cSettings
       createDirectoryIfMissing True dir
       writeFile (dir </> prefix ++ ".c") $ cmacros ++ cfile
       writeFile (dir </> prefix ++ ".h") hfile

-- | Compile a specification to a .h and a .c file.
--
-- The first argument is used as prefix for the .h and .c files generated.
compile :: String -> Spec -> IO ()
compile = compileWith mkDefaultCSettings

-- | Generate the .c file from a 'Spec'.
--
-- The generated C file has the following structure:
--
-- * Include .h file.
-- * Declarations of global buffers and indices.
-- * Generator functions for streams, guards and trigger arguments.
-- * Declaration of the @step()@ function.
compilec :: CSettings -> Spec -> C.TransUnit
compilec cSettings spec = C.TransUnit declns funs where
  streams  = specStreams spec
  triggers = specTriggers spec
  exts     = gatherexts streams triggers
  exprs    = gatherexprs streams triggers

  declns = mkstructdeclns exprs ++ mkexts exts ++ mkglobals streams
  funs   = genfuns streams triggers ++ [mkstep cSettings streams triggers exts]

  -- Write struct datatypes
  mkstructdeclns :: [UExpr] -> [C.Decln]
  mkstructdeclns es = catMaybes $ map mkdecln utypes where
    mkdecln (UType ty) = case ty of
      Struct x -> Just $ mkstructdecln ty
      _        -> Nothing

    utypes = nub $ concatMap (\(UExpr _ e) -> exprtypes e) es

  -- Make declarations for copies of external variables.
  mkexts :: [External] -> [C.Decln]
  mkexts exts = map mkextcpydecln exts

  -- Make buffer and index declarations for streams.
  mkglobals :: [Stream] -> [C.Decln]
  mkglobals streams = map buffdecln streams ++ map indexdecln streams where
    buffdecln  (Stream sid buff _ ty) = mkbuffdecln  sid ty buff
    indexdecln (Stream sid _    _ _ ) = mkindexdecln sid

  -- Make generator functions, including trigger arguments.
  genfuns :: [Stream] -> [Trigger] -> [C.FunDef]
  genfuns streams triggers =  map accessdecln streams
                           ++ map streamgen streams
                           ++ concatMap triggergen triggers where

    accessdecln :: Stream -> C.FunDef
    accessdecln (Stream sid buff _ ty) = mkaccessdecln sid ty buff

    streamgen :: Stream -> C.FunDef
    streamgen (Stream sid _ expr ty) = genfun (generatorname sid) expr ty

    triggergen :: Trigger -> [C.FunDef]
    triggergen (Trigger name guard args) = guarddef : argdefs where
      guarddef = genfun (guardname name) guard Bool
      argdefs  = map arggen (zip (argnames name) args)

      arggen :: (String, UExpr) -> C.FunDef
      arggen (argname, UExpr ty expr) = genfun argname expr ty

-- | Generate the .h file from a 'Spec'.
compileh :: CSettings -> Spec -> C.TransUnit
compileh cSettings spec = C.TransUnit declns [] where
  streams  = specStreams spec
  triggers = specTriggers spec
  exts     = gatherexts streams triggers
  exprs    = gatherexprs streams triggers

  declns =  mkstructforwdeclns exprs
         ++ mkexts exts
         ++ extfundeclns triggers
         ++ [stepdecln]

  mkstructforwdeclns :: [UExpr] -> [C.Decln]
  mkstructforwdeclns es = catMaybes $ map mkdecln utypes where
    mkdecln (UType ty) = case ty of
      Struct x -> Just $ mkstructforwdecln ty
      _        -> Nothing

    utypes = nub $ concatMap (\(UExpr _ e) -> exprtypes e) es

  -- Make declarations for external variables.
  mkexts :: [External] -> [C.Decln]
  mkexts = map mkextdecln

  extfundeclns :: [Trigger] -> [C.Decln]
  extfundeclns triggers = map extfundecln triggers where
    extfundecln :: Trigger -> C.Decln
    extfundecln (Trigger name _ args) = C.FunDecln Nothing cty name params where
        cty    = C.TypeSpec C.Void
        params = map mkparam $ zip (argnames name) args
        mkparam (name, UExpr ty _) = C.Param (transtype ty) name

  -- Declaration for the step function.
  stepdecln :: C.Decln
  stepdecln = C.FunDecln Nothing (C.TypeSpec C.Void)
                  (cSettingsStepFunctionName cSettings) []
