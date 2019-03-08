{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
mkextcpydecln (External name cpyname ty) = decln where
  cty   = transtype ty
  decln = C.Decln (Just C.Static) cty cpyname init
  init  = mkinit ty (defaultval ty) where
    -- Make a default init value based on the type.
    -- Arrays recurse on their contents type.
    -- Structs unpack the struct, and replace every value with the default.
    -- Note that the name of the field (s) have to be copied explicitly.
    defaultval :: Type a -> a
    defaultval ty = case ty of
      Bool      -> False
      Int8      -> 0
      Int16     -> 0
      Int32     -> 0
      Int64     -> 0
      Word8     -> 0
      Word16    -> 0
      Word32    -> 0
      Word64    -> 0
      Float     -> 0.0
      Double    -> 0.0
      Array ty' -> (array $ take (tylength ty) $ repeat $ defaultval ty')
      Struct s  -> fromValues $ map mkval (toValues s) where
        mkval (Value ty' (_ :: Field s t)) = Value ty' (field ty' :: Field s t)
        field ty' = Field $ defaultval ty'

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

