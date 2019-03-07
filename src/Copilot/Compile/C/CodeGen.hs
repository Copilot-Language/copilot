{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.CodeGen where

import Text.PrettyPrint     (render)
import Control.Monad.State  (runState)

import Language.C99.Pretty  (pretty)
import qualified Language.C99.Simple as C

import Copilot.Core
import Copilot.Compile.C.Util
import Copilot.Compile.C.External
import Copilot.Compile.C.Translate


-- | Compile the specification to a .h and a .c file.
compile :: Spec -> String -> IO ()
compile spec prefix = do
  let cfile = render $ pretty $ C.translate $ compilec spec
      hfile = render $ pretty $ C.translate $ compileh spec

  writeFile (prefix ++ ".c") cfile
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

  declns = mkexts exts ++ mkglobals streams
  funs   = genfuns streams triggers

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
      argdefs  = map arggen (zip argnames args)

      argnames = [aname | n <- [0..], let aname = argname name n]

      arggen :: (String, UExpr) -> C.FunDef
      arggen (argname, UExpr ty expr) = genfun argname expr ty

compileh :: Spec -> C.TransUnit
compileh = undefined

-- | Write a generator function for a stream.
genfun :: String -> Expr a -> Type a -> C.FunDef
genfun name expr ty = C.FunDef cty name [] cvars [C.Return $ Just cexpr] where
  cty = transtype ty
  (cexpr, (cvars, _)) = runState (transexpr expr) mempty

-- | Make a declaration for a copy of an external variable.
mkextcpydecln :: External -> C.Decln
mkextcpydecln = undefined

-- | Make a C buffer variable and initialise it with the stream buffer.
mkbuffdecln :: Id -> Type a -> [a] -> C.Decln
mkbuffdecln sid ty xs = C.Decln (Just C.Static) cty name initvals where
  name     = buffername sid
  cty      = C.Array (transtype ty) (Just $ C.LitInt $ fromIntegral buffsize)
  buffsize = length xs
  initvals = mkinits ty xs

-- | Make a C index variable and initialise it to 0.
mkindexdecln :: Id -> C.Decln
mkindexdecln sid = C.Decln (Just C.Static) cty name initval where
  name    = indexname sid
  cty     = C.TypeSpec $ C.TypedefName "size_t"
  initval = C.InitExpr $ C.LitInt 0

-- | Make an initial declaration from a list of values.
mkinits :: Type a -> [a] -> C.Init
mkinits ty xs = C.InitArray $ map (mkinit ty) xs

-- | Make an initial declaration from a single value.
mkinit :: Type a -> a -> C.Init
mkinit (Array ty') xs = C.InitArray $ map (mkinit ty') (arrayelems xs)
mkinit ty          x  = C.InitExpr  $ constty ty x
