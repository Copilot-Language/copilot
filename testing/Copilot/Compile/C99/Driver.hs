{-# LANGUAGE GADTs #-}

module Copilot.Compile.C99.Driver (writedriver) where

import Data.List                      (intersperse)

import Language.C99.Simple as C99

import Copilot.Core       as Core
  ( Spec    (..)
  , UExpr   (..)
  , Trigger (..)
  , Type    (..)
  , Value   (..)
  , toValues
  , fieldname
  , tylength
  )
import Copilot.Compile.C99.Translate  (transtype)
import Copilot.Compile.C99.Util       (argnames)

import Text.PrettyPrint               (render)
import Language.C99.Simple            (translate)
import Language.C99.Pretty            (pretty)


-- | Write the complete driver .c file code based on the specification.
writedriver :: String -> Spec -> Int -> String
writedriver specname spec iters = unlines (includes) ++ code
  where
    includes = [ "#include <stdio.h>"
               , "#include <stdint.h>"
               , "#include <stdbool.h>"
               , "#include \"" ++ specname ++ ".h\""
               ]
    code = render $ pretty $ translate $ mktransunit spec iters


-- | Write a C driver mimicking Copilot's interpreter.
mktransunit :: Spec -> Int -> TransUnit
mktransunit spec iters = TransUnit vardefs fundefs
  where
    vardefs   = []
    fundefs   = ctriggers ++ [ mkmain iters spec ]
    ctriggers = map mktrigger (specTriggers spec)


-- | Write a trigger function which prints all its arguments in a CSV like
-- format.
mktrigger :: Trigger -> FunDef
mktrigger (Trigger name guard args) = FunDef returntype name params [] body
  where
    returntype = TypeSpec Void
    namedargs  = zip (argnames name) args
    params     = map mkparam namedargs
    body       = [Expr $ mkprintfcsv name namedargs]

    mkparam :: (String, UExpr) -> Param
    mkparam (name, UExpr ty _) = Param (transtype ty) name


-- | Write the main function. The purpose of this function is to call the
-- step-function a number of times.
mkmain :: Int -> Spec -> FunDef
mkmain iters spec = FunDef (TypeSpec Int) "main" params decln body
  where
    params = [ Param (TypeSpec Int) "argc"
             , Param (Const $ C99.Array (Ptr $ TypeSpec Char) Nothing) "argv"
             ]
    decln  = [VarDecln Nothing (TypeSpec Int) "i" (Just $ InitExpr $ LitInt 0)]
    body   = [For (Ident "i" .= LitInt 0)
                  (Ident "i" .< LitInt (fromIntegral iters))
                  (UnaryOp Inc (Ident "i"))
                  [ Expr $ Funcall (Ident "printf") [LitString "#\n"]
                  , Expr $ Funcall (Ident "step") []
                  ]
             ]


-- | Write a call to printf with a format resembling CSV.
mkprintfcsv :: String -> [(String, UExpr)] -> Expr
mkprintfcsv trigname namedargs = Funcall (Ident "printf") (fmt:vals)
  where
    fmt    = LitString $ trigname ++ "," ++ concat (intersperse "," argfmt) ++ "\n"
    argfmt = map (uexprfmt.snd) namedargs
    vals   = map (\(name, UExpr ty _) -> mkidents name ty) namedargs

    uexprfmt :: UExpr -> String
    uexprfmt (UExpr ty _) = tyfmt ty

    tyfmt :: Core.Type a -> String
    tyfmt ty = case ty of
      Core.Bool       -> "%s"
      Core.Int8       -> "%d"
      Core.Int16      -> "%d"
      Core.Int32      -> "%d"
      Core.Int64      -> "%ld"
      Core.Word8      -> "%u"
      Core.Word16     -> "%u"
      Core.Word32     -> "%u"
      Core.Word64     -> "%lu"
      Core.Float      -> "%f"
      Core.Double     -> "%f"
      Core.Struct s   -> "<" ++ elems ++ ">"
        where
          elems = concat $ intersperse "," $ map fieldfmt (toValues s)
          fieldfmt :: Core.Value a -> String
          fieldfmt (Core.Value ty f) = Core.fieldname f ++ ":" ++ tyfmt ty
      Core.Array  ty' -> "[" ++ elems ++ "]"
        where
          elems = concat $ intersperse "," $ map tyfmt types
          types = replicate (Core.tylength ty) ty'

    mkidents :: String -> Core.Type a -> Expr
    mkidents name ty = case ty of
      Core.Struct _ -> error "mkidents: Struct not implemented yet."
      Core.Array  _ -> error "mkindents: Array not implemented yet."
      Core.Bool     -> Cond (Ident name) (LitString "true") (LitString "false")
      _             -> Ident name
