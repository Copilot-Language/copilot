{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.CodeGen where

import Text.PrettyPrint     (render)
import Language.C99.Pretty  (pretty)
import qualified Language.C99.Simple as C

import Copilot.Core
import Copilot.Compile.C.Util
import Copilot.Compile.C.Translate


-- | Compile the specification to a .h and a .c file.
compile :: Spec -> String -> IO ()
compile spec prefix = do
  let cfile = render $ pretty $ C.translate $ compilec spec
      hfile = render $ pretty $ C.translate $ compileh spec

  writeFile (prefix ++ ".c") cfile
  writeFile (prefix ++ ".h") hfile

compilec :: Spec -> C.TransUnit
compilec = undefined

compileh :: Spec -> C.TransUnit
compileh = undefined


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
