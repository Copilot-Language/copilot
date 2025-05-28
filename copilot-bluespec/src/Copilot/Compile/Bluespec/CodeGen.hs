{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-level translation of Copilot Core into Bluespec.
module Copilot.Compile.Bluespec.CodeGen
  ( -- * Type declarations
    mkStructDecln

    -- * Ring buffers
  , mkBuffDecln
  , mkIndexDecln
  , mkAccessDecln

    -- * Stream generators
  , mkGenFun

    -- * Monitor processing
  , mkStepRule
  , mkTriggerRule

    -- * Module interface specifications
  , mkSpecIfcFields
  ) where

-- External imports
import Data.String (IsString (..))
import qualified Language.Bluespec.Classic.AST as BS
import qualified Language.Bluespec.Classic.AST.Builtin.Ids as BS
import qualified Language.Bluespec.Classic.AST.Builtin.Types as BS

-- Internal imports: Copilot
import Copilot.Core

-- Internal imports
import Copilot.Compile.Bluespec.Expr
import Copilot.Compile.Bluespec.External
import Copilot.Compile.Bluespec.Name
import Copilot.Compile.Bluespec.Representation
import Copilot.Compile.Bluespec.Type

-- | Write a generator function for a stream.
mkGenFun :: String -> Expr a -> Type a -> BS.CDefl
mkGenFun name expr ty =
    -- name :: ty
    -- name = expr
    BS.CLValueSign
      (BS.CDef nameId (BS.CQType [] (transType ty)) [def])
      []
  where
    nameId = BS.mkId BS.NoPos $ fromString $ lowercaseName name
    def = BS.CClause [] [] (transExpr expr)

-- | Bind a buffer variable and initialise it with the stream buffer.
mkBuffDecln :: forall a. Id -> Type a -> [a] -> [BS.CStmt]
mkBuffDecln sId ty xs =
    initVals ++ [BS.CSletrec [initBufSig]]
  where
    -- sId_0     :: Reg <ty> <- mkReg xs_0
    -- ...
    -- sId_(n-1) :: Reg <ty> <- mkReg xs_(n-1)
    initVals = zipWith mkInitVal xs [0..]
    -- sId :: Vector n (Reg <ty>)
    -- sId = update (... (update newVector 0 sId_0) ...) (n-1) sId_(n-1)
    initBufSig = BS.CLValueSign
                   (BS.CDef nameId (BS.CQType [] vecTy) [initBufDef])
                   []
    initBufDef = BS.CClause
                   []
                   []
                   (genVector
                     (\idx _ -> BS.CVar $ BS.mkId BS.NoPos $
                                fromString $ streamElemName sId idx)
                     xs)

    nameId   = BS.mkId BS.NoPos $ fromString $ streamName sId
    bsTy     = tReg `BS.TAp` transType ty
    vecTy    = tVector `BS.TAp` BS.cTNum numElems BS.NoPos `BS.TAp` bsTy
    numElems = toInteger $ length xs

    mkInitVal :: a -> Int -> BS.CStmt
    mkInitVal x elemNum =
        BS.CSBindT
          (BS.CPVar elemId)
          Nothing
          []
          (BS.CQType [] bsTy)
          (BS.CApply (BS.CVar (BS.mkId BS.NoPos "mkReg")) [constTy ty x])
      where
        elemName = streamElemName sId elemNum
        elemId   = BS.mkId BS.NoPos $ fromString elemName

-- | Make an index variable and initialise it to 0.
mkIndexDecln :: Id -> BS.CStmt
mkIndexDecln sId =
  -- sId_idx :: Reg (Bit 64) <- mkReg 0
  BS.CSBindT
    (BS.CPVar nameId)
    Nothing
    []
    (BS.CQType [] bsTy)
    (BS.CApply (BS.CVar (BS.mkId BS.NoPos "mkReg"))
               [cLit $ BS.LInt $ BS.ilDec 0])
  where
    nameId = BS.mkId BS.NoPos $ fromString $ indexName sId
    bsTy   = tReg `BS.TAp` BS.tBitN 64 BS.NoPos

-- | Define an accessor function for the ring buffer associated with a stream
mkAccessDecln :: Id -> Type a -> [a] -> BS.CDefl
mkAccessDecln sId ty xs =
    -- sId_get :: Bits 64 -> ty
    -- sId_get x = (select sId ((sId_idx + x) % buffLength))._read
    BS.CLValueSign (BS.CDef nameId (BS.CQType [] funTy) [def]) []
  where
    def        = BS.CClause [BS.CPVar argId] [] expr
    argTy      = BS.tBit `BS.TAp` BS.cTNum 64 BS.NoPos
    retTy      = transType ty
    funTy      = BS.tArrow `BS.TAp` argTy `BS.TAp` retTy
    name       = streamAccessorName sId
    nameId     = BS.mkId BS.NoPos $ fromString name
    buffLength = cLit $ BS.LInt $ BS.ilDec $ toInteger $ length xs
    argId      = BS.mkId BS.NoPos "x"
    index      = BS.CApply (BS.CVar (BS.idPercentAt BS.NoPos))
                   [ BS.CApply (BS.CVar BS.idPlus)
                       [ BS.CVar (BS.mkId BS.NoPos (fromString (indexName sId)))
                       , BS.CVar argId
                       ]
                   , buffLength
                   ]
    indexExpr  = cIndexVector
                   (BS.CVar (BS.mkId BS.NoPos (fromString (streamName sId))))
                   index
    expr       = BS.CSelect indexExpr (BS.id_read BS.NoPos)

-- | Define fields for a module interface containing a specification's trigger
-- functions and external variables.
mkSpecIfcFields :: [Trigger] -> [External] -> [BS.CField]
mkSpecIfcFields triggers exts =
    map mkTriggerField triggers ++ map mkExtField exts
  where
    -- trigger :: args_1 -> ... -> args_n -> Action
    mkTriggerField :: Trigger -> BS.CField
    mkTriggerField (Trigger name _ args) =
      mkField name $
      foldr
        (\(UExpr arg _) res -> BS.tArrow `BS.TAp` transType arg `BS.TAp` res)
        BS.tAction
        args

    -- ext :: Reg ty
    mkExtField :: External -> BS.CField
    mkExtField (External name ty) =
      mkField name $ tReg `BS.TAp` transType ty

-- | Define a rule for a trigger function.
mkTriggerRule :: UniqueTrigger -> BS.CRule
mkTriggerRule (UniqueTrigger uniqueName (Trigger name _ args)) =
    BS.CRule
      []
      (Just $ cLit $ BS.LString uniqueName)
      [ BS.CQFilter $
        BS.CVar $ BS.mkId BS.NoPos $
        fromString $ guardName uniqueName
      ]
      (BS.CApply nameExpr args')
  where
    ifcArgId = BS.mkId BS.NoPos $ fromString ifcArgName
    -- Note that we use 'name' here instead of 'uniqueName', as 'name' is the
    -- name of the actual external function.
    nameId   = BS.mkId BS.NoPos $ fromString $ lowercaseName name
    nameExpr = BS.CSelect (BS.CVar ifcArgId) nameId

    args'   = take (length args) (map argCall (argNames uniqueName))
    argCall = BS.CVar . BS.mkId BS.NoPos . fromString

-- | Writes the @step@ rule that updates all streams.
mkStepRule :: [Stream] -> Maybe BS.CRule
mkStepRule streams
  | null allUpdates
  = -- If there is nothing to update, don't bother creating a step rule.
    -- Doing so wouldn't harm anything, but bsc will generate a warning
    -- when compiling such an empty rule.
    Nothing
  | otherwise
  = Just $
    BS.CRule
      []
      (Just $ cLit $ BS.LString "step")
      [BS.CQFilter $ BS.CCon BS.idTrue []]
      (BS.Caction BS.NoPos allUpdates)
  where
    allUpdates = bufferUpdates ++ indexUpdates
    (bufferUpdates, indexUpdates) = unzip $ map mkUpdateGlobals streams

    -- Write code to update global stream buffers and index.
    mkUpdateGlobals :: Stream -> (BS.CStmt, BS.CStmt)
    mkUpdateGlobals (Stream sId buff _ _) =
        (bufferUpdate, indexUpdate)
      where
        bufferUpdate =
          BS.CSExpr Nothing $
          BS.Cwrite
            BS.NoPos
            (cIndexVector (BS.CVar buffId) (BS.CVar indexId))
            (BS.CVar genId)

        indexUpdate =
          BS.CSExpr Nothing $
          BS.Cwrite
            BS.NoPos
            (BS.CVar indexId)
            (BS.CApply (BS.CVar (BS.idPercentAt BS.NoPos))
                       [incIndex, buffLength])
          where
            buffLength = cLit $ BS.LInt $ BS.ilDec $ toInteger $ length buff
            incIndex   = BS.CApply (BS.CVar BS.idPlus)
                           [ BS.CVar indexId
                           , cLit $ BS.LInt $ BS.ilDec 1
                           ]

        buffId  = BS.mkId BS.NoPos $ fromString $ streamName sId
        genId   = BS.mkId BS.NoPos $ fromString $ generatorName sId
        indexId = BS.mkId BS.NoPos $ fromString $ indexName sId

-- | Write a struct declaration based on its definition.
mkStructDecln :: Struct a => a -> BS.CDefn
mkStructDecln x =
    BS.Cstruct
      True
      BS.SStruct
      (BS.IdK structId)
      [] -- No type variables
      structFields
      -- Derive a Bits instance so that we can put this struct in a Reg
      [BS.CTypeclass BS.idBits]
  where
    structId = BS.mkId BS.NoPos $ fromString $ uppercaseName $ typeName x
    structFields = map mkStructField $ toValues x

    mkStructField :: Value a -> BS.CField
    mkStructField (Value ty field) =
      mkField (fieldName field) (transType ty)

-- | Write a field of a struct or interface, along with its type.
mkField :: String -> BS.CType -> BS.CField
mkField name ty =
  BS.CField
    { BS.cf_name = BS.mkId BS.NoPos $ fromString $ lowercaseName name
    , BS.cf_pragmas = Nothing
    , BS.cf_type = BS.CQType [] ty
    , BS.cf_default = []
    , BS.cf_orig_type = Nothing
    }

-- | The @Reg@ Bluespec interface type.
tReg :: BS.CType
tReg = BS.TCon $
  BS.TyCon
    { BS.tcon_name = BS.idReg
    , BS.tcon_kind = Just (BS.Kfun BS.KStar BS.KStar)
    , BS.tcon_sort = BS.TIstruct (BS.SInterface [])
                                 [BS.id_write BS.NoPos, BS.id_read BS.NoPos]
    }
