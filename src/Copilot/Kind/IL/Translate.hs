--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes, NamedFieldPuns, ScopedTypeVariables, GADTs,
             LambdaCase #-}

module Copilot.Kind.IL.Translate ( translate ) where

import Copilot.Kind.IL.Spec
import Copilot.Kind.Misc.Cast
import Copilot.Kind.CoreUtils.Operators

import qualified Copilot.Core as C

import qualified Data.Map.Strict as Map

import Control.Applicative ((<$>), (<*))
import Control.Monad.State

import Data.Char

import Text.Printf

--------------------------------------------------------------------------------

-- 'nc' stands for naming convention.
ncSeq :: C.Id -> SeqId
ncSeq = printf "s%d"

-- We assume all local variables have distinct names whatever their scopes.
ncLocal :: C.Name -> SeqId
ncLocal s = "l" ++ dropWhile (not . isNumber) s

ncExternVar :: C.Name -> SeqId
ncExternVar n = "ext_" ++ n

ncExternFun :: C.Name -> SeqId
ncExternFun n = "_" ++ n

ncUnhandledOp :: String -> String
ncUnhandledOp = id

ncFresh :: Integer -> SeqId
ncFresh n = "f" ++ show n

--------------------------------------------------------------------------------

-- | Translates a Copilot specification to an IL specification

translate :: C.Spec -> IL
translate (C.Spec {C.specStreams, C.specProperties}) = runTrans $ do

  let modelInit = concatMap streamInit specStreams

  mainConstraints <- mapM streamRec specStreams

  localConstraints <- getLocalConstraints
  properties <- Map.fromList <$>
    forM specProperties
      (\(C.Property {C.propertyName, C.propertyExpr}) -> do
        e' <- expr Bool propertyExpr
        propConds <- getLocalConstraints
        return (propertyName, (propConds, e')))

  return IL
    { modelInit
    , modelRec = mainConstraints ++ localConstraints
    , properties
    , inductive = not $ null specStreams
    }

bound :: Expr b -> C.Type a -> Trans ()
bound s t = case typeOf s of
  Integer -> case t of
    C.Int8    -> bound' s C.Int8
    C.Int16   -> bound' s C.Int16
    C.Int32   -> bound' s C.Int32
    C.Int64   -> bound' s C.Int64
    C.Word8   -> bound' s C.Word8
    C.Word16  -> bound' s C.Word16
    C.Word32  -> bound' s C.Word32
    C.Word64  -> bound' s C.Word64
    _         -> return ()
  _         -> return ()
  where bound' :: (Num b, Ord a, Bounded a) => Expr b -> C.Type a -> Trans ()
        bound' var t = localConstraint (Op2 Bool And
            (Op2 Bool Le (Const (typeOf var) $
              cast (typeOf var) $ toDyn t minBound) var)
            (Op2 Bool Ge (Const (typeOf var) $
              cast (typeOf var) $ toDyn t maxBound) var))

streamInit :: C.Stream -> [Constraint]
streamInit (C.Stream { C.streamId       = id
                     , C.streamBuffer   = b :: [val]
                     , C.streamExprType = ty }) =

  zipWith initConstraint [0..] b
  where
    initConstraint :: Integer -> val -> Constraint
    initConstraint p v = casting ty $ \ty' ->
      Op2 Bool Eq
        (SVal ty' (ncSeq id) (Fixed p))
        (Const ty' $ cast ty' $ toDyn ty v)

streamRec :: C.Stream -> Trans Constraint
streamRec (C.Stream { C.streamId       = id
                    , C.streamExpr     = e
                    , C.streamBuffer   = b
                    , C.streamExprType = ty})
  = casting ty $ \ty' -> do
      let s = SVal ty' (ncSeq id) (_n_plus $ length b)
      bound s ty
      e' <- expr ty' e
      return $ Op2 Bool Eq s e'

--------------------------------------------------------------------------------

expr :: Type t -> C.Expr a -> Trans (Expr t)

expr t (C.Const ct v) = return $ case (t, ct) of
  (Integer, C.Int8)   -> negifyI t ct v
  (Integer, C.Int16)  -> negifyI t ct v
  (Integer, C.Int32)  -> negifyI t ct v
  (Integer, C.Int64)  -> negifyI t ct v
  (_, C.Float)        -> negify t ct v
  (_, C.Double)       -> negify t ct v
  _                   -> Const t (cast t $ toDyn ct v)
  where negify :: (Ord a, Num a) => Type b -> C.Type a -> a -> Expr b
        negify t ct v
            | v >= 0    = Const t (cast t $ toDyn ct v)
            | otherwise = Op1 t Neg $ Const t (cast t $ toDyn ct (-v))
        negifyI :: (Integral a, Integral b) => Type b -> C.Type a -> a -> Expr b
        negifyI t _ v
            | v >= 0    = ConstI t $ toInteger v
            -- TODO(chathhorn): somehow handle this in a cleaner way. Flipping
            -- the sign can take the value out of the representable range in
            -- the case of fixed-width ints.
            | otherwise = Op1 t Neg $ ConstI t $ negate $ toInteger v

expr t (C.Label _ _ e) = expr t e

expr t (C.Drop _ k id) = return $ SVal t (ncSeq id) (_n_plus k)

expr t (C.Local ta _ name ea eb) = casting ta $ \ta' -> do
  ea' <- expr ta' ea
  localConstraint (Op2 Bool Eq (SVal ta' (ncLocal name) _n_) ea')
  expr t eb

expr t (C.Var _ name) = return $ SVal t (ncLocal name) _n_

expr t (C.ExternVar ct name _) = bound s ct >> return s
  where s = SVal t (ncExternVar name) _n_

expr t (C.ExternFun ct name args _ _) = do
  args' <- mapM trArg args
  let s = FunApp t (ncExternFun name) args'
  bound s ct
  return s
  where trArg (C.UExpr {C.uExprExpr, C.uExprType}) = casting uExprType $ \ta ->
          U <$> expr ta uExprExpr

-- Arrays and functions are treated the same way
expr t (C.ExternArray ta tb name _ ind _ _) =
  expr t (C.ExternFun tb name [C.UExpr ta ind] Nothing Nothing)

expr t (C.Op1 (C.Sqrt ta) e) = expr t (C.Op2 (C.Pow ta) e (C.Const ta 0.5))

expr t (C.Op1 (C.Sign ta) e) = case ta of
  C.Int8   -> trSign t ta e
  C.Int16  -> trSign t ta e
  C.Int32  -> trSign t ta e
  C.Int64  -> trSign t ta e
  C.Float  -> trSign t ta e
  C.Double -> trSign t ta e
  _        -> expr t $ C.Const ta 1
  where trSign :: (Ord t, Num t) => Type t' -> C.Type t -> C.Expr t -> Trans (Expr t')
        trSign t ta e =
          expr t (C.Op3 (C.Mux ta)
            (C.Op2 (C.Lt ta) e (C.Const ta 0))
            (C.Const ta (-1))
            (C.Op3 (C.Mux ta)
              (C.Op2 (C.Gt ta) e (C.Const ta 0))
              (C.Const ta 1)
              (C.Const ta 0)))

expr t (C.Op1 op e) = handleOp1 t (op, e) expr notHandled Op1
  where notHandled (UnhandledOp1 opName ta _) = do
          e' <- U <$> expr ta e
          return $ FunApp t (ncUnhandledOp opName) [e']

expr t (C.Op2 op e1 e2) = handleOp2 t (op, e1, e2) expr notHandled Op2 (Op1 Bool Not)
  where notHandled (UnhandledOp2 opName ta tb _) = do
          e1' <- U <$> expr ta e1
          e2' <- U <$> expr tb e2
          return $ FunApp t (ncUnhandledOp opName) [e1', e2']

expr t (C.Op3 (C.Mux _) cond e1 e2) = do
  cond' <- expr Bool cond
  e1'   <- expr t    e1
  e2'   <- expr t    e2
  f     <- fresh
  let s = SVal t f _n_
  localConstraint $ Op2 Bool Or (Op1 Bool Not cond') (Op2 Bool Eq s e1')
  localConstraint $ Op2 Bool Or cond' (Op2 Bool Eq s e2')
  return s

--------------------------------------------------------------------------------

mkVarName :: String -> [U Expr] -> String
mkVarName name args = name ++ "_" ++ concatMap argToString args

argToString :: U Expr -> String
argToString (U (Const Integer x)) = show x
argToString (U (Op2 _ Add e1 e2)) = argToString (U e1) ++ argToString (U e2)
argToString (U _) = error "translating arg to string (should never happen)"

data TransST = TransST
  { localConstraints :: [Constraint]
  , nextFresh        :: Integer
  }

type Trans = State TransST

localConstraint :: Constraint -> Trans ()
localConstraint c =
  modify $ \st -> st {localConstraints = c : localConstraints st}

fresh :: Trans String
fresh = do
  modify $ \st -> st {nextFresh = nextFresh st + 1}
  n <- nextFresh <$> get
  return $ ncFresh n

getLocalConstraints :: Trans [Constraint]
getLocalConstraints = (localConstraints <$> get) <*
  (modify $ \st -> st {localConstraints = []})

runTrans :: Trans a -> a
runTrans m = evalState m $ TransST [] 0

--------------------------------------------------------------------------------
