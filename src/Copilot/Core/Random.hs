--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Copilot.Core.Random
  ( randomSpec
  , randomExtVals ) where

import Copilot.Core 
import Copilot.Core.Interpret.Eval (ExtEnv (..), Env)
import Copilot.Core.Random.Gen
import Copilot.Core.Random.Weights
import Copilot.Core.Type.Dynamic
import Copilot.Core.Type.Equality ((=~=))

import Control.Monad
import Data.List (foldl', nubBy)
import Prelude hiding (id)
import System.Random (StdGen)
import Control.Monad.Reader

--------------------------------------------------------------------------------

randomSpec :: Weights -> StdGen -> Spec
randomSpec = runGen genSpec 0

--------------------------------------------------------------------------------

genSpec :: Gen Spec
genSpec = do
  ws          <- weights
  numTriggers <- choose (1, maxTriggers ws)
  ss          <- genStreamInfo's
  extVars     <- genExtVars
  streams     <- runReaderT (mapM (genStream ss) ss) extVars
  triggers    <- 
    runReaderT (mapM (genTrigger ss) (map mkTriggerName [0..numTriggers-1]))
               extVars
  return Spec { specStreams   = streams
              , specObservers = []
              , specTriggers  = triggers }
  where
  mkTriggerName :: Int -> Name
  mkTriggerName k = "f" ++ show k

  -- Possibly generate external variables to be shared among streams.
  genExtVars :: Gen [DynExtVar]
  genExtVars = do
    ws <- weights
    n  <- choose (0, maxExtVars ws)
    let lst = [1..n] -- empty list if n == 0
    mapM genExtVar lst
    where
    genExtVar :: Int -> Gen DynExtVar
    genExtVar i = do 
      ws <- weights                 
      WrapType t <- genType ws
      let expr = ExternVar t ("ext" ++ show i)
      return $ toDynF t expr

--------------------------------------------------------------------------------

data StreamInfo = forall a . (Eq a, Ord a) => StreamInfo
  { streamInfoId         :: Id
  , streamInfoType       :: Type a
  , streamInfoBufferSize :: Int }

--------------------------------------------------------------------------------

data WrapType = forall a . (Eq a, Ord a) => WrapType (Type a)

genType :: Weights -> Gen WrapType
genType ws = freq
  [ (boolFreq   ws, return $ WrapType (typeOf :: Type Bool  ))
  , (int8Freq   ws, return $ WrapType (typeOf :: Type Int8  ))
  , (int16Freq  ws, return $ WrapType (typeOf :: Type Int16 ))
  , (int32Freq  ws, return $ WrapType (typeOf :: Type Int32 ))
  , (int64Freq  ws, return $ WrapType (typeOf :: Type Int64 ))
  , (word8Freq  ws, return $ WrapType (typeOf :: Type Word8 ))
  , (word16Freq ws, return $ WrapType (typeOf :: Type Word16))
  , (word32Freq ws, return $ WrapType (typeOf :: Type Word32))
  , (word64Freq ws, return $ WrapType (typeOf :: Type Word64))
  , (floatFreq  ws, return $ WrapType (typeOf :: Type Float ))
  , (floatFreq  ws, return $ WrapType (typeOf :: Type Double)) ]

genTypeFromStreamInfo's :: [StreamInfo] -> Gen WrapType
genTypeFromStreamInfo's = elements . map (\(StreamInfo _ t _) -> WrapType t)

--------------------------------------------------------------------------------

genStreamInfo's :: Gen [StreamInfo]
genStreamInfo's = do
  let s0 = StreamInfo 0 (typeOf :: Type Bool) 1
  ws      <- weights
  ss      <- mapM genStreamInfo [1 .. numStreams ws - 1]
  return (s0 : ss)

  where
  genStreamInfo :: Int -> Gen StreamInfo
  genStreamInfo id = do
    ws <- weights
    k  <- choose (1, maxBuffSize ws)
    WrapType t <- genType ws
    return
      StreamInfo
        { streamInfoId         = id
        , streamInfoType       = t
        , streamInfoBufferSize = k }

--------------------------------------------------------------------------------

-- External variables
type DynExtVar = DynamicF Expr Type

type StreamEnv = ReaderT [DynExtVar] Gen

--------------------------------------------------------------------------------

genStream :: [StreamInfo] -> StreamInfo -> StreamEnv Stream
genStream ss
  StreamInfo
    { streamInfoId         = id
    , streamInfoType       = t
    , streamInfoBufferSize = k } 
  = do
  xs <- lift $ replicateM k (randomFromType t)
  extVars <- ask
  w  <- lift $ genExpr extVars ss t
  return 
      Stream
        { streamId       = id
        , streamBuffer   = xs
        , streamGuard    = Const (typeOf :: Type Bool) True
        , streamExpr     = w
        , streamExprType = t }

--------------------------------------------------------------------------------

genTrigger :: [StreamInfo] -> Name -> StreamEnv Trigger
genTrigger ss name = do
  extVars <- ask
  w  <- lift $ genExpr extVars ss (typeOf :: Type Bool)
  ws <- lift $ weights
  i  <- lift $ choose (1, maxTrigArgs ws)
  args <- replicateM i genArg
  return
    Trigger
      { triggerName  = name
      , triggerGuard = w
      , triggerArgs  = args }

  where
  genArg :: StreamEnv UExpr
  genArg = do
    WrapType t <- lift $ genTypeFromStreamInfo's ss
    extVars <- ask
    w <- lift $ genExpr extVars ss t
    return
      UExpr
        { uExprExpr = w
        , uExprType = t }

--------------------------------------------------------------------------------

genExpr :: [DynExtVar] -> [StreamInfo] -> Type a -> Gen (Expr a)
genExpr extVars ss t = do
  dp <- depth
  ws <- weights
  if dp >= maxExprDepth ws
    then freq (terminalLst ws)
    else freq $ 
      terminalLst ws ++
      [ (op1Freq   ws, genOp1)
      , (op2Freq   ws, genOp2)
      , (op3Freq   ws, genOp3) ]

  where
  terminalLst ws = 
    (extVarFreq' ws, genExtVar) : [ (constFreq ws, genConst)
                                  , (dropFreq  ws, genDrop) ] 

  extVarFreq' ws = if null typedExtVars then 0 else extVarFreq ws
  
  genExtVar = do
    let vars = typedExtVars
    r <- choose (0, length vars - 1)
    return (vars !! r)

  typedExtVars = foldl' typedExtVar [] extVars
    where typedExtVar vars dyn = 
            case fromDynF t dyn of
                   Nothing   -> vars
                   Just expr -> expr:vars

  genConst = do 
    x <- randomFromType t 
    return $ Const t x

  genDrop = do
    s <- findStreamInfoWithMatchingType
    k <- choose (0, streamInfoBufferSize s - 1)
    return $ Drop t (fromIntegral k) (streamInfoId s)
    where
    findStreamInfoWithMatchingType =
      let p (StreamInfo _ t1 _) = case t =~= t1 of
                                    Just _ -> True
                                    _      -> False
      in  elements (filter p ss)

  genOp3 = incDepth (genOp3Mux extVars ss t)    

  genOp1 = incDepth $ case t of
    Bool   -> genOp1Bool extVars ss
    Int8   -> genOp1Num  extVars ss t
    Int16  -> genOp1Num  extVars ss t
    Int32  -> genOp1Num  extVars ss t
    Int64  -> genOp1Num  extVars ss t
    Word8  -> genOp1Num  extVars ss t
    Word16 -> genOp1Num  extVars ss t
    Word32 -> genOp1Num  extVars ss t
    Word64 -> genOp1Num  extVars ss t
    Float  -> genOp1Num  extVars ss t
    Double -> genOp1Num  extVars ss t

  genOp2 = incDepth $ case t of
    Bool    -> oneOf [ genOp2Bool extVars ss
                     , genOp2Eq extVars ss
                     , genOp2Ord extVars ss 
                     ]
    Int8    -> intOrWord NumWit IntegralWit
    Int16   -> intOrWord NumWit IntegralWit
    Int32   -> intOrWord NumWit IntegralWit
    Int64   -> intOrWord NumWit IntegralWit
    Word8   -> intOrWord NumWit IntegralWit
    Word16  -> intOrWord NumWit IntegralWit
    Word32  -> intOrWord NumWit IntegralWit
    Word64  -> intOrWord NumWit IntegralWit
    Float   -> floatOrDouble NumWit
    Double  -> floatOrDouble NumWit

    where
    floatOrDouble numWit = oneOf [ genOp2Num extVars ss t numWit ]

    intOrWord numWit integralWit = do 
      ws <- weights 
      if divModFreq ws 
        then oneOf $ num ++ [ genOp2Integral extVars ss t integralWit ]
        else oneOf num
      where 
      num = [ genOp2Num extVars ss t numWit ]
      

--------------------------------------------------------------------------------

genOp1Bool :: [DynExtVar] -> [StreamInfo] -> Gen (Expr Bool)
genOp1Bool extVars ss = do
  ew <- genExpr extVars ss (typeOf :: Type Bool)
  return $ Op1 Not ew

genOp1Num :: Num a => [DynExtVar] -> [StreamInfo] -> Type a -> Gen (Expr a)
genOp1Num extVars ss t = do
  ew  <- genExpr extVars ss t
  opw <- elements [Abs t, Sign t]
  return $ Op1 opw ew

genOp2Bool :: [DynExtVar] -> [StreamInfo] -> Gen (Expr Bool)
genOp2Bool extVars ss = do
  ew1 <- genExpr extVars ss (typeOf :: Type Bool)
  ew2 <- genExpr extVars ss (typeOf :: Type Bool)
  opw <- elements [And, Or]
  return $ Op2 opw ew1 ew2

genOp2Eq :: [DynExtVar] -> [StreamInfo] -> Gen (Expr Bool)
genOp2Eq extVars ss = do
  WrapType t <- genTypeFromStreamInfo's ss
  ew1 <- genExpr extVars ss t
  ew2 <- genExpr extVars ss t
  opw <- elements [Eq t, Ne t]
  return $ Op2 opw ew1 ew2

genOp2Ord :: [DynExtVar] -> [StreamInfo] -> Gen (Expr Bool)
genOp2Ord extVars ss = 
  let ss' = findStreamOmittingType Bool in
  if (null ss') then genExpr extVars ss Bool 
    else do
      WrapType t <- genTypeFromStreamInfo's ss'
      ew1 <- genExpr extVars ss t
      ew2 <- genExpr extVars ss t
      opw <- elements [ (Lt t)
                      , (Gt t)
                      , (Le t)
                      , (Ge t) ]
      return $ Op2 opw ew1 ew2
  where
  findStreamOmittingType :: Type a -> [StreamInfo]
  findStreamOmittingType t0 = 
    let p (StreamInfo _ t1 _) = case t0 =~= t1 of
                                  Just _ -> True
                                  _      -> False
    in  filter (not . p) ss

genOp2Num :: [DynExtVar] -> [StreamInfo] -> Type a -> NumWit a -> Gen (Expr a)
genOp2Num extVars ss t NumWit = do
  ew1 <- genExpr extVars ss t
  ew2 <- genExpr extVars ss t
  opw <- elements [ (Add t)
                  , (Sub t)
                  , (Mul t) ]
  return $ Op2 opw ew1 ew2

genOp2Integral :: 
  [DynExtVar] -> [StreamInfo] -> Type a -> IntegralWit a -> Gen (Expr a)
genOp2Integral extVars ss t IntegralWit = do
  ew1 <- genExpr extVars ss t
  ew2 <- genExpr extVars ss t
  opw <- elements [ (Div t)
                  , (Mod t) ]
  return $ Op2 opw ew1 ew2

genOp3Mux :: [DynExtVar] -> [StreamInfo] -> Type a -> Gen (Expr a)
genOp3Mux extVars ss t = do
  ew1 <- genExpr extVars ss (typeOf :: Type Bool)
  ew2 <- genExpr extVars ss t
  ew3 <- genExpr extVars ss t
  return $ Op3 (Mux t) ew1 ew2 ew3

data NumWit a = Num a => NumWit

data IntegralWit a = Integral a => IntegralWit

--------------------------------------------------------------------------------

randomExtVals :: Int -> Spec -> Weights -> StdGen -> ExtEnv
randomExtVals rnds spec = runGen env 0
  where env = do vars <- extVals rnds spec
                 return ExtEnv { varEnv = vars
                               , arrEnv = []
                               , funcEnv = []
                               }

--------------------------------------------------------------------------------

-- Extract the external variables, returning randomly-generated lists for their
-- values.
extVals :: Int -> Spec -> Gen (Env Name)
extVals rnds Spec { specStreams = strms
                  , specTriggers = triggers } 
  = 
  let vars = nubBy (\a b -> fst a == fst b) $ 
               concatMap strmExts strms ++ 
               concatMap triggerExts triggers  in
  mapM randomLst vars

  where 
  randomLst :: (Name, UType) -> Gen (Name, DynamicF [] Type)
  randomLst (nm, UType { uTypeType = t }) = do
    rnd <- randomReplicate rnds t
    return $ (nm, toDynF t rnd)

  strmExts Stream { streamExpr = expr } = extsFromExpr expr

  triggerExts Trigger { triggerGuard = grd
                      , triggerArgs = args } = 
    extsFromExpr grd ++ concatMap getExpr args
      where getExpr UExpr { uExprExpr = expr } = extsFromExpr expr
  
  extsFromExpr :: Expr a -> [(Name, UType)]
  extsFromExpr expr =
    case expr of
      Const _ _              -> []
      Drop _ _ _             -> []
      Local _ _ _ e1 e2      -> extsFromExpr e1 ++ extsFromExpr e2
      Var _ _                -> []
      ExternVar t name       -> [(name, UType { uTypeType = t })]
      ExternFun _ _ _ _      -> []
      ExternArray _ _ _ _ _  -> []
      Op1 _ e                -> extsFromExpr e
      Op2 _ e1 e2            -> extsFromExpr e1 ++ extsFromExpr e2
      Op3 _ e1 e2 e3         -> extsFromExpr e1 ++ extsFromExpr e2
                                  ++ extsFromExpr e3

--------------------------------------------------------------------------------
