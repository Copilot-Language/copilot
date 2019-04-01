module Copilot.Compile.C99.Compile
  ( compile
  ) where

import Text.PrettyPrint     (render)
import Data.List            (nub)
import Data.Maybe           (catMaybes)

import Language.C99.Pretty  (pretty)
import qualified Language.C99.Simple as C

import Copilot.Core
import Copilot.Compile.C99.Util
import Copilot.Compile.C99.External
import Copilot.Compile.C99.Translate
import Copilot.Compile.C99.CodeGen

-- | Compile the specification to a .h and a .c file.
compile :: String -> Spec -> IO ()
compile prefix spec = do
  let cfile = render $ pretty $ C.translate $ compilec spec
      hfile = render $ pretty $ C.translate $ compileh spec

      -- TODO: find a nicer solution using annotated AST's
      cmacros = unlines [ "#include <stdint.h>"
                        , "#include <stdbool.h>"
                        , "#include <string.h>"
                        , ""
                        , "#include \"" ++ prefix ++ ".h\""
                        , ""
                        ]

  writeFile (prefix ++ ".c") $ cmacros ++ cfile
  writeFile (prefix ++ ".h") hfile

-- | Generate the .c file from a spec. It has the following structure:
-- |
-- | * Include .h file
-- | * Declarations of global buffers and indices.
-- | * Generator functions for streams, guards and trigger args.
-- | * Declaration of step() function.
compilec :: Spec -> C.TransUnit
compilec spec = C.TransUnit declns funs where
  streams  = specStreams spec
  triggers = specTriggers spec
  exts     = gatherexts streams triggers
  exprs    = gatherexprs streams triggers

  declns = mkstructdeclns exprs ++ mkexts exts ++ mkglobals streams
  funs   = genfuns streams triggers ++ [mkstep streams triggers exts]

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
  genfuns streams triggers =  map streamgen streams
                           ++ concatMap triggergen triggers where
    streamgen :: Stream -> C.FunDef
    streamgen (Stream sid _ expr ty) = genfun (generatorname sid) expr ty

    triggergen :: Trigger -> [C.FunDef]
    triggergen (Trigger name guard args) = guarddef : argdefs where
      guarddef = genfun (guardname name) guard Bool
      argdefs  = map arggen (zip (argnames name) args)

      arggen :: (String, UExpr) -> C.FunDef
      arggen (argname, UExpr ty expr) = genfun argname expr ty

-- | Generate the .h file from a spec.
compileh :: Spec -> C.TransUnit
compileh spec = C.TransUnit declns [] where
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
  stepdecln = C.FunDecln Nothing (C.TypeSpec C.Void) "step" []
