{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Copilot.Compile.C99.CodeGen where

import Control.Monad.State  (runState)
import Data.List            (union)
import Data.Typeable        (Typeable)

import qualified Language.C99.Simple as C

import Copilot.Core
import Copilot.Compile.C99.Util
import Copilot.Compile.C99.External
import Copilot.Compile.C99.Translate

-- | Write a declaration for a generator function.
gendecln :: String -> Type a -> C.Decln
gendecln name ty = C.FunDecln Nothing cty name [] where
  cty = C.decay $ transtype ty

-- | Write a generator function for a stream.
genfun :: String -> Expr a -> Type a -> C.FunDef
genfun name expr ty = C.FunDef cty name [] cvars [C.Return $ Just cexpr] where
  cty = C.decay $ transtype ty
  (cexpr, (cvars, _)) = runState (transexpr expr) mempty

-- | Make a extern declaration of an variable.
mkextdecln :: External -> C.Decln
mkextdecln (External name _ ty) = decln where
  decln = C.VarDecln (Just C.Extern) cty name Nothing
  cty   = transtype ty

-- | Make a declaration for a copy of an external variable.
mkextcpydecln :: External -> C.Decln
mkextcpydecln (External name cpyname ty) = decln where
  cty   = transtype ty
  decln = C.VarDecln (Just C.Static) cty cpyname Nothing

-- | Make a C buffer variable and initialise it with the stream buffer.
mkbuffdecln :: Id -> Type a -> [a] -> C.Decln
mkbuffdecln sid ty xs = C.VarDecln (Just C.Static) cty name initvals where
  name     = streamname sid
  cty      = C.Array (transtype ty) (Just $ C.LitInt $ fromIntegral buffsize)
  buffsize = length xs
  initvals = Just $ C.InitArray $ map (C.InitExpr . constty ty) xs

-- | Make a C index variable and initialise it to 0.
mkindexdecln :: Id -> C.Decln
mkindexdecln sid = C.VarDecln (Just C.Static) cty name initval where
  name    = indexname sid
  cty     = C.TypeSpec $ C.TypedefName "size_t"
  initval = Just $ C.InitExpr $ C.LitInt 0

-- | The step function updates all streams,a
mkstep :: [Stream] -> [Trigger] -> [External] -> C.FunDef
mkstep streams triggers exts = C.FunDef void "step" [] declns stmts where
  void = C.TypeSpec C.Void
  declns = []
  stmts  =  map mkexcopy exts
         ++ map mktriggercheck triggers
         ++ bufferupdates
         ++ indexupdates
  (bufferupdates, indexupdates) = unzip $ map mkupdateglobals streams

  -- Write code to global stream buffers and index.
  mkupdateglobals :: Stream -> (C.Stmt, C.Stmt)
  mkupdateglobals (Stream sid buff expr ty) = (bufferupdate, indexupdate)
    where
      bufferupdate = case ty of
        Array _ -> C.Expr $ memcpy dest val size
          where
            dest  = C.Index buff_var index_var
            size = C.LitInt $ fromIntegral $ tysize ty
        _       -> C.Expr $ C.Index buff_var index_var C..= val

      indexupdate = C.Expr $ index_var C..= (incindex C..% bufflength)
        where
          bufflength = C.LitInt $ fromIntegral $ length buff
          incindex   = (C..++) index_var

      buff_var  = C.Ident $ streamname sid
      index_var = C.Ident $ indexname sid
      val       = C.Funcall (C.Ident $ generatorname sid) []

  -- Make code that copies an external variable to its local one.
  mkexcopy :: External -> C.Stmt
  mkexcopy (External name cpyname ty) = C.Expr $ case ty of
    Array _ -> memcpy exvar locvar size where
                 exvar  = C.Ident cpyname
                 locvar = C.Ident name
                 size   = C.LitInt $ fromIntegral $ tysize ty
    _       -> C.Ident cpyname C..= C.Ident name

  -- Make if-statement to check the guard, call the trigger if necessary.
  mktriggercheck :: Trigger -> C.Stmt
  mktriggercheck (Trigger name guard args) = C.If guard' firetrigger where
    guard'      = C.Funcall (C.Ident $ guardname name) []
    firetrigger = [C.Expr $ C.Funcall (C.Ident name) args'] where
      args'        = take (length args) (map argcall (argnames name))
      argcall name = C.Funcall (C.Ident name) []

  -- Write a call to the memcpy function.
  memcpy :: C.Expr -> C.Expr -> C.Expr -> C.Expr
  memcpy dest src size = C.Funcall (C.Ident "memcpy") [dest, src, size]


-- | Write a struct declaration based on its definition.
mkstructdecln :: Struct a => Type a -> C.Decln
mkstructdecln (Struct x) = C.TypeDecln struct where
  struct = C.TypeSpec $ C.StructDecln (Just $ typename x) fields
  fields = map mkfield (toValues x)

  mkfield :: Value a -> C.FieldDecln
  mkfield (Value ty field) = C.FieldDecln (transtype ty) (fieldname field)

-- | Write a forward struct decralration.
mkstructforwdecln :: Struct a => Type a -> C.Decln
mkstructforwdecln (Struct x) = C.TypeDecln struct where
  struct = C.TypeSpec $ C.Struct (typename x)

-- | List all types of an expression, returns items uniquely.
exprtypes :: Typeable a => Expr a -> [UType]
exprtypes e = case e of
  Const ty _            -> typetypes ty
  Local ty1 ty2 _ e1 e2 -> typetypes ty1 `union` typetypes ty2
                           `union` exprtypes e1 `union` exprtypes e2
  Var ty _              -> typetypes ty
  Drop ty _ _           -> typetypes ty
  ExternVar ty _ _      -> typetypes ty
  Op1 _ e1              -> exprtypes e1
  Op2 _ e1 e2           -> exprtypes e1 `union` exprtypes e2
  Op3 _ e1 e2 e3        -> exprtypes e1 `union` exprtypes e2 `union` exprtypes e3
  Label ty _ _          -> typetypes ty

-- | List all types of an type, returns items uniquely.
typetypes :: Typeable a => Type a -> [UType]
typetypes ty = case ty of
  Array ty' -> [UType ty] `union` typetypes ty'
  Struct x  -> [UType ty] `union` map (\(Value ty' _) -> UType ty') (toValues x)
  _         -> [UType ty]

-- | Collect all expression of a list of streams and triggers and wrap them
-- into an UEXpr.
gatherexprs :: [Stream] -> [Trigger] -> [UExpr]
gatherexprs streams triggers =  map streamexpr streams
                             ++ concatMap triggerexpr triggers where
  streamexpr  (Stream _ _ expr ty)   = UExpr ty expr
  triggerexpr (Trigger _ guard args) = UExpr Bool guard : args

