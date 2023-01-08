{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-level translation of Copilot Core into C99.
module Copilot.Compile.C99.CodeGen where

import           Control.Monad.State (runState)
import           Data.List           (union, unzip4)
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Typeable       (Typeable)

import qualified Language.C99.Simple as C

import Copilot.Core
import Copilot.Compile.C99.Error     (impossible)
import Copilot.Compile.C99.Util
import Copilot.Compile.C99.External
import Copilot.Compile.C99.Settings
import Copilot.Compile.C99.Translate

-- | Write a declaration for a generator function.
gendecln :: String -> Type a -> C.Decln
gendecln name ty = C.FunDecln Nothing cty name []
  where
    cty = C.decay $ transtype ty

-- | Write a generator function for a stream.
genfun :: String -> Expr a -> Type a -> C.FunDef
genfun name expr ty = C.FunDef cty name [] cvars [C.Return $ Just cexpr]
  where
    cty = C.decay $ transtype ty
    (cexpr, cvars) = runState (transexpr expr) mempty

-- | Write a generator function for a stream that returns an array.
genFunArray :: String -> String -> Expr a -> Type a -> C.FunDef
genFunArray name nameArg expr ty@(Array _) =
    C.FunDef funType name [ outputParam ] varDecls stmts
  where
    funType = C.TypeSpec C.Void

    -- The output value is an array
    outputParam = C.Param cArrayType nameArg
    cArrayType  = transtype ty

    -- Output value, and any variable declarations needed
    (cexpr, varDecls) = runState (transexpr expr) mempty

    -- Copy expression to output argument
    stmts = [ C.Expr $ memcpy (C.Ident nameArg) cexpr size ]
    size  = C.LitInt (fromIntegral $ tysize ty)
              C..* C.SizeOfType (C.TypeName $ tyElemName ty)

genFunArray name nameArg expr _ =
  impossible "genFunArray" "copilot-c99"

-- | Make a extern declaration of a variable.
mkextdecln :: External -> C.Decln
mkextdecln (External name _ ty) = decln
  where
    decln = C.VarDecln (Just C.Extern) cty name Nothing
    cty   = transtype ty

-- | Make a declaration for a copy of an external variable.
mkextcpydecln :: External -> C.Decln
mkextcpydecln (External name cpyname ty) = decln
  where
    cty   = transtype ty
    decln = C.VarDecln (Just C.Static) cty cpyname Nothing

-- | Make a C buffer variable and initialise it with the stream buffer.
mkbuffdecln :: Id -> Type a -> [a] -> C.Decln
mkbuffdecln sid ty xs = C.VarDecln (Just C.Static) cty name initvals
  where
    name     = streamname sid
    cty      = C.Array (transtype ty) (Just $ C.LitInt $ fromIntegral buffsize)
    buffsize = length xs
    initvals = Just $ C.InitList $ constarray ty xs

-- | Make a C index variable and initialise it to 0.
mkindexdecln :: Id -> C.Decln
mkindexdecln sid = C.VarDecln (Just C.Static) cty name initval
  where
    name    = indexname sid
    cty     = C.TypeSpec $ C.TypedefName "size_t"
    initval = Just $ C.InitExpr $ C.LitInt 0

-- | Define an accessor functions for the ring buffer associated with a stream
mkaccessdecln :: Id -> Type a -> [a] -> C.FunDef
mkaccessdecln sid ty xs = C.FunDef cty name params [] [C.Return (Just expr)]
  where
    cty        = C.decay $ transtype ty
    name       = streamaccessorname sid
    bufflength = C.LitInt $ fromIntegral $ length xs
    params     = [C.Param (C.TypeSpec $ C.TypedefName "size_t") "x"]
    index      = (C.Ident (indexname sid) C..+ C.Ident "x") C..% bufflength
    expr       = C.Index (C.Ident (streamname sid)) index

-- | Writes the step function, that updates all streams.
mkstep :: CSettings -> [Stream] -> [Trigger] -> [External] -> C.FunDef
mkstep cSettings streams triggers exts =
    C.FunDef void (cSettingsStepFunctionName cSettings) [] declns stmts
  where

    void = C.TypeSpec C.Void
    stmts  =  map mkexcopy exts
           ++ triggerStmts
           ++ tmpassigns
           ++ bufferupdates
           ++ indexupdates
    declns =  streamDeclns
           ++ concat triggerDeclns
    (streamDeclns, tmpassigns, bufferupdates, indexupdates) =
      unzip4 $ map mkupdateglobals streams
    (triggerDeclns, triggerStmts) =
      unzip $ map mktriggercheck triggers

    -- Write code to update global stream buffers and index.
    mkupdateglobals :: Stream -> (C.Decln, C.Stmt, C.Stmt, C.Stmt)
    mkupdateglobals (Stream sid buff expr ty) =
      (tmpdecln, tmpassign, bufferupdate, indexupdate)
        where
          tmpdecln = C.VarDecln Nothing cty tmp_var Nothing

          tmpassign = case ty of
            Array _ -> C.Expr $ C.Funcall (C.Ident $ generatorname sid)
                                          [ C.Ident tmp_var ]
            _       -> C.Expr $ C.Ident tmp_var C..= val

          bufferupdate = case ty of
            Array _ -> C.Expr $ memcpy dest (C.Ident tmp_var) size
              where
                dest = C.Index buff_var index_var
                size = C.LitInt (fromIntegral $ tysize ty)
                         C..* C.SizeOfType (C.TypeName (tyElemName ty))
            _       -> C.Expr $
                           C.Index buff_var index_var C..= (C.Ident tmp_var)

          indexupdate = C.Expr $ index_var C..= (incindex C..% bufflength)
            where
              bufflength = C.LitInt $ fromIntegral $ length buff
              incindex   = index_var C..+ C.LitInt 1

          tmp_var   = streamname sid ++ "_tmp"
          buff_var  = C.Ident $ streamname sid
          index_var = C.Ident $ indexname sid
          val       = C.Funcall (C.Ident $ generatorname sid) []
          cty       = transtype ty

    -- Make code that copies an external variable to its local one.
    mkexcopy :: External -> C.Stmt
    mkexcopy (External name cpyname ty) = C.Expr $ case ty of
      Array _ -> memcpy exvar locvar size
        where
          exvar  = C.Ident cpyname
          locvar = C.Ident name
          size   = C.LitInt (fromIntegral $ tysize ty)
                     C..* C.SizeOfType (C.TypeName (tyElemName ty))

      _       -> C.Ident cpyname C..= C.Ident name

    -- Make if-statement to check the guard, call the handler if necessary.
    -- This returns two things:
    --
    -- * A list of Declns for temporary variables, one for each argument that
    --   the handler function accepts. For example, if a handler function takes
    --   three arguments, the list of Declns might look something like this:
    --
    --   @
    --   int8_t   handler_arg_temp0;
    --   int16_t  handler_arg_temp1;
    --   struct s handler_arg_temp2;
    --   @
    --
    -- * A Stmt representing the if-statement. Continuing the example above,
    --   the if-statement would look something like this:
    --
    --   @
    --   if (handler_guard()) {
    --     handler_arg_temp0 = handler_arg0();
    --     handler_arg_temp1 = handler_arg1();
    --     handler_arg_temp2 = handler_arg2();
    --     handler(handler_arg_temp0, handler_arg_temp1, &handler_arg_temp2);
    --   }
    --   @
    --
    -- We create temporary variables because:
    --
    -- 1. We want to pass structs by reference intead of by value. To this end,
    --    we use C's & operator to obtain a reference to a temporary variable
    --    of a struct type and pass that to the handler function.
    --
    -- 2. Assigning a struct to a temporary variable defensively ensures that
    --    any modifications that the handler called makes to the struct argument
    --    will not affect the internals of the monitoring code.
    mktriggercheck :: Trigger -> ([C.Decln], C.Stmt)
    mktriggercheck (Trigger name guard args) =
        (aTmpDeclns, ifStmt)
      where
        aTmpDeclns = zipWith (\tmpVar arg ->
                               C.VarDecln Nothing (tempType arg) tmpVar Nothing)
                             aTempNames
                             args
          where
            tempType (UExpr { uExprType = ty }) =
              case ty of
                -- If a temporary variable is being used to store an array,
                -- declare the type of the temporary variable as a pointer, not
                -- an array. The problem with declaring it as an array is that
                -- the `arg` function will return a pointer, not an array, and
                -- C doesn't make it easy to cast directly from an array to a
                -- pointer.
                Array ty' -> C.Ptr $ transtype ty'
                _         -> transtype ty

        aTempNames = take (length args) (argTempNames name)

        ifStmt = C.If guard' firetrigger

        guard' = C.Funcall (C.Ident $ guardname name) []

        -- The body of the if-statement. This consists of statements that assign
        -- the values of the temporary variables, following by a final statement
        -- that passes the temporary variables to the handler function.
        firetrigger = map C.Expr argAssigns ++
                      [C.Expr $ C.Funcall (C.Ident name)
                                          (zipWith passArg aTempNames args)]
          where
            passArg aTempName (UExpr { uExprType = ty }) =
              case ty of
                -- Special case for Struct to pass reference to temporary
                -- struct variable to handler. (See the comments for
                -- mktriggercheck for details.)
                Struct _ -> C.UnaryOp C.Ref $ C.Ident aTempName
                _        -> C.Ident aTempName

            argAssigns = zipWith (\aTempName arg ->
                                   C.AssignOp C.Assign (C.Ident aTempName) arg)
                                 aTempNames
                                 args'
            args'        = take (length args) (map argcall (argnames name))
            argcall name = C.Funcall (C.Ident name) []


-- | Write a struct declaration based on its definition.
mkstructdecln :: Struct a => Type a -> C.Decln
mkstructdecln (Struct x) = C.TypeDecln struct
  where
    struct = C.TypeSpec $ C.StructDecln (Just $ typename x) fields
    fields = NonEmpty.fromList $ map mkfield (toValues x)

    mkfield :: Value a -> C.FieldDecln
    mkfield (Value ty field) = C.FieldDecln (transtype ty) (fieldname field)

-- | Write a forward struct declaration.
mkstructforwdecln :: Struct a => Type a -> C.Decln
mkstructforwdecln (Struct x) = C.TypeDecln struct
  where
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

-- | List all types of a type, returns items uniquely.
typetypes :: Typeable a => Type a -> [UType]
typetypes ty = case ty of
  Array ty' -> typetypes ty' `union` [UType ty]
  Struct x  -> concatMap (\(Value ty' _) -> typetypes ty') (toValues x) `union` [UType ty]
  _         -> [UType ty]

-- | Collect all expression of a list of streams and triggers and wrap them
-- into an UEXpr.
gatherexprs :: [Stream] -> [Trigger] -> [UExpr]
gatherexprs streams triggers =  map streamexpr streams
                             ++ concatMap triggerexpr triggers
  where
    streamexpr  (Stream _ _ expr ty)   = UExpr ty expr
    triggerexpr (Trigger _ guard args) = UExpr Bool guard : args

-- * Auxiliary functions

-- Write a call to the memcpy function.
memcpy :: C.Expr -> C.Expr -> C.Expr -> C.Expr
memcpy dest src size = C.Funcall (C.Ident "memcpy") [dest, src, size]

-- Translate a Copilot type to a C99 type, handling arrays especially.
--
-- If the given type is an array (including multi-dimensional arrays), the
-- type is that of the elements in the array. Otherwise, it is just the
-- equivalent representation of the given type in C.
tyElemName :: Type a -> C.Type
tyElemName ty = case ty of
  Array ty' -> tyElemName ty'
  _         -> transtype ty
