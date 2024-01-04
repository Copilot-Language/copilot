{-# LANGUAGE GADTs #-}

-- | High-level translation of Copilot Core into C99.
module Copilot.Compile.C99.CodeGen
    (
      -- * Externs
      mkExtCpyDecln
    , mkExtDecln

      -- * Type declarations
    , mkStructDecln
    , mkStructForwDecln

      -- * Ring buffers
    , mkBuffDecln
    , mkIndexDecln
    , mkAccessDecln

      -- * Stream generators
    , mkGenFun
    , mkGenFunArray

      -- * Monitor processing
    , mkStep
    )
  where

-- External imports
import           Control.Monad.State ( runState )
import           Data.List           ( unzip4 )
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Language.C99.Simple as C

-- Internal imports: Copilot
import Copilot.Core ( Expr (..), Id, Stream (..), Struct (..), Trigger (..),
                      Type (..), UExpr (..), Value (..), fieldName, typeSize )

-- Internal imports
import Copilot.Compile.C99.Error    ( impossible )
import Copilot.Compile.C99.Expr     ( constArray, transExpr )
import Copilot.Compile.C99.External ( External (..) )
import Copilot.Compile.C99.Name     ( argNames, argTempNames, generatorName,
                                      guardName, indexName, streamAccessorName,
                                      streamName )
import Copilot.Compile.C99.Settings ( CSettings, cSettingsStepFunctionName )
import Copilot.Compile.C99.Type     ( transType )

-- * Externs

-- | Make a extern declaration of a variable.
mkExtDecln :: External -> C.Decln
mkExtDecln (External name _ ty) = decln
  where
    decln = C.VarDecln (Just C.Extern) cTy name Nothing
    cTy   = transType ty

-- | Make a declaration for a copy of an external variable.
mkExtCpyDecln :: External -> C.Decln
mkExtCpyDecln (External _name cpyName ty) = decln
  where
    decln = C.VarDecln (Just C.Static) cTy cpyName Nothing
    cTy   = transType ty

-- * Type declarations

-- | Write a struct declaration based on its definition.
mkStructDecln :: Struct a => Type a -> C.Decln
mkStructDecln (Struct x) = C.TypeDecln struct
  where
    struct = C.TypeSpec $ C.StructDecln (Just $ typeName x) fields
    fields = NonEmpty.fromList $ map mkField (toValues x)

    mkField :: Value a -> C.FieldDecln
    mkField (Value ty field) = C.FieldDecln (transType ty) (fieldName field)

-- | Write a forward struct declaration.
mkStructForwDecln :: Struct a => Type a -> C.Decln
mkStructForwDecln (Struct x) = C.TypeDecln struct
  where
    struct = C.TypeSpec $ C.Struct (typeName x)

-- * Ring buffers

-- | Make a C buffer variable and initialise it with the stream buffer.
mkBuffDecln :: Id -> Type a -> [a] -> C.Decln
mkBuffDecln sId ty xs = C.VarDecln (Just C.Static) cTy name initVals
  where
    name     = streamName sId
    cTy      = C.Array (transType ty) (Just $ C.LitInt $ fromIntegral buffSize)
    buffSize = length xs
    initVals = Just $ C.InitList $ constArray ty xs

-- | Make a C index variable and initialise it to 0.
mkIndexDecln :: Id -> C.Decln
mkIndexDecln sId = C.VarDecln (Just C.Static) cTy name initVal
  where
    name    = indexName sId
    cTy     = C.TypeSpec $ C.TypedefName "size_t"
    initVal = Just $ C.InitExpr $ C.LitInt 0

-- | Define an accessor functions for the ring buffer associated with a stream.
mkAccessDecln :: Id -> Type a -> [a] -> C.FunDef
mkAccessDecln sId ty xs =
    C.FunDef Nothing cTy name params [] [C.Return (Just expr)]
  where
    cTy        = C.decay $ transType ty
    name       = streamAccessorName sId
    buffLength = C.LitInt $ fromIntegral $ length xs
    params     = [C.Param (C.TypeSpec $ C.TypedefName "size_t") "x"]
    index      = (C.Ident (indexName sId) C..+ C.Ident "x") C..% buffLength
    expr       = C.Index (C.Ident (streamName sId)) index

-- * Stream generators

-- | Write a generator function for a stream.
mkGenFun :: String -> Expr a -> Type a -> C.FunDef
mkGenFun name expr ty =
    C.FunDef Nothing cTy name [] cVars [C.Return $ Just cExpr]
  where
    cTy            = C.decay $ transType ty
    (cExpr, cVars) = runState (transExpr expr) mempty

-- | Write a generator function for a stream that returns an array.
mkGenFunArray :: String -> String -> Expr a -> Type a -> C.FunDef
mkGenFunArray name nameArg expr ty@(Array _) =
    C.FunDef Nothing funType name [ outputParam ] varDecls stmts
  where
    funType = C.TypeSpec C.Void

    -- The output value is an array
    outputParam = C.Param cArrayType nameArg
    cArrayType  = transType ty

    -- Output value, and any variable declarations needed
    (cExpr, varDecls) = runState (transExpr expr) mempty

    -- Copy expression to output argument
    stmts = [ C.Expr $ memcpy (C.Ident nameArg) cExpr size ]
    size  = C.LitInt (fromIntegral $ typeSize ty)
              C..* C.SizeOfType (C.TypeName $ tyElemName ty)

mkGenFunArray _name _nameArg _expr _ty =
  impossible "mkGenFunArray" "copilot-c99"

-- * Monitor processing

-- | Define the step function that updates all streams.
mkStep :: CSettings -> [Stream] -> [Trigger] -> [External] -> C.FunDef
mkStep cSettings streams triggers exts =
    C.FunDef Nothing void (cSettingsStepFunctionName cSettings) [] declns stmts
  where
    void = C.TypeSpec C.Void

    declns =  streamDeclns
           ++ concat triggerDeclns

    stmts  =  map mkExCopy exts
           ++ triggerStmts
           ++ tmpAssigns
           ++ bufferUpdates
           ++ indexUpdates

    (streamDeclns, tmpAssigns, bufferUpdates, indexUpdates) =
      unzip4 $ map mkUpdateGlobals streams
    (triggerDeclns, triggerStmts) =
      unzip $ map mkTriggerCheck triggers

    -- Update the value of a variable with the result of calling a function that
    -- generates the next value in a stream expression. If the type of the
    -- variable is an array, then we cannot perform a direct C assignment, so
    -- we instead pass the variable as an output array to the function.
    updateVar :: C.Ident -> C.Ident -> Type a -> C.Expr
    updateVar varName genName (Array _) =
      C.Funcall (C.Ident genName) [C.Ident varName]
    updateVar varName genName _ =
      C.AssignOp C.Assign (C.Ident varName) (C.Funcall (C.Ident genName) [])

    -- Write code to update global stream buffers and index.
    mkUpdateGlobals :: Stream -> (C.Decln, C.Stmt, C.Stmt, C.Stmt)
    mkUpdateGlobals (Stream sId buff _expr ty) =
      (tmpDecln, tmpAssign, bufferUpdate, indexUpdate)
        where
          tmpDecln = C.VarDecln Nothing cTy tmpVar Nothing

          tmpAssign = C.Expr $ updateVar tmpVar (generatorName sId) ty

          bufferUpdate = case ty of
            Array _ -> C.Expr $ memcpy dest (C.Ident tmpVar) size
              where
                dest = C.Index buffVar indexVar
                size = C.LitInt
                           (fromIntegral $ typeSize ty)
                           C..* C.SizeOfType (C.TypeName (tyElemName ty))
            _       -> C.Expr $
                           C.Index buffVar indexVar C..= C.Ident tmpVar

          indexUpdate = C.Expr $ indexVar C..= (incIndex C..% buffLength)
            where
              buffLength = C.LitInt $ fromIntegral $ length buff
              incIndex   = indexVar C..+ C.LitInt 1

          tmpVar   = streamName sId ++ "_tmp"
          buffVar  = C.Ident $ streamName sId
          indexVar = C.Ident $ indexName sId
          cTy      = transType ty

    -- Make code that copies an external variable to its local one.
    mkExCopy :: External -> C.Stmt
    mkExCopy (External name cpyName ty) = C.Expr $ case ty of
      Array _ -> memcpy exVar locVar size
        where
          exVar  = C.Ident cpyName
          locVar = C.Ident name
          size   = C.LitInt (fromIntegral $ typeSize ty)
                     C..* C.SizeOfType (C.TypeName (tyElemName ty))

      _       -> C.Ident cpyName C..= C.Ident name

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
    mkTriggerCheck :: Trigger -> ([C.Decln], C.Stmt)
    mkTriggerCheck (Trigger name _guard args) =
        (aTmpDeclns, triggerCheckStmt)
      where
        aTmpDeclns :: [C.Decln]
        aTmpDeclns = zipWith declare args aTempNames
          where
            declare :: UExpr -> C.Ident -> C.Decln
            declare (UExpr { uExprType = ty }) tmpVar =
              C.VarDecln Nothing (transType ty) tmpVar Nothing

        triggerCheckStmt :: C.Stmt
        triggerCheckStmt = C.If guard' fireTrigger
          where
            guard' = C.Funcall (C.Ident $ guardName name) []

            -- The body of the if-statement. This consists of statements that
            -- assign the values of the temporary variables, following by a
            -- final statement that passes the temporary variables to the
            -- handler function.
            fireTrigger =  map C.Expr argAssigns
                        ++ [C.Expr $
                               C.Funcall (C.Ident name)
                                         (zipWith passArg aTempNames args)]
              where
                -- List of assignments of values of temporary variables.
                argAssigns :: [C.Expr]
                argAssigns = zipWith3 assign aTempNames aArgNames args

                assign :: C.Ident -> C.Ident -> UExpr -> C.Expr
                assign aTempName aArgName (UExpr { uExprType = ty }) =
                  updateVar aTempName aArgName ty

                aArgNames :: [C.Ident]
                aArgNames = take (length args) (argNames name)

                -- Build an expression to pass a temporary variable as argument
                -- to a trigger handler.
                --
                -- We need to pass a reference to the variable in some cases,
                -- so we also need the type of the expression, which is enclosed
                -- in the second argument, an UExpr.
                passArg :: String -> UExpr -> C.Expr
                passArg aTempName (UExpr { uExprType = ty }) =
                  case ty of
                    -- Special case for Struct to pass reference to temporary
                    -- struct variable to handler. (See the comments for
                    -- mktriggercheck for details.)
                    Struct _ -> C.UnaryOp C.Ref $ C.Ident aTempName
                    _        -> C.Ident aTempName

        aTempNames :: [String]
        aTempNames = take (length args) (argTempNames name)

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
  _         -> transType ty
