--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Copilot.Core.Random
  ( randomSpec
  ) where

import Control.Monad
import Copilot.Core (UExpr (..))
import qualified Copilot.Core as E
import Copilot.Core.Spec
import Copilot.Core.Random.Gen
import Copilot.Core.Random.Weights
import Copilot.Core.Type
import Copilot.Core.Type.Equality
import Data.Int
import Data.Word
import Prelude hiding (id)
import System.Random (StdGen)

--------------------------------------------------------------------------------

randomSpec :: Weights -> StdGen -> Spec
randomSpec = runGen genSpec 0

--------------------------------------------------------------------------------

genSpec :: Gen Spec
genSpec =
  do
    ws <- weights
    numTriggers <- choose (1, maxTriggers ws)
    ss <- genStreamInfo's
    streams  <- mapM (genStream ss) ss
    triggers <- mapM (genTrigger ss) (map mkTriggerName [0..numTriggers-1])
    return
      Spec
        { specStreams   = streams
        , specObservers = []
        , specTriggers  = triggers }

  where

  mkTriggerName :: Int -> E.Name
  mkTriggerName k = "f" ++ show k

--------------------------------------------------------------------------------

data StreamInfo = forall a . (Eq a, Ord a) => StreamInfo
  { streamInfoId         :: E.Id
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
genTypeFromStreamInfo's = elements . map (\ (StreamInfo _ t _) -> WrapType t)

--------------------------------------------------------------------------------

genStreamInfo's :: Gen [StreamInfo]
genStreamInfo's =
  do
    let
      s0 = StreamInfo 0 (typeOf :: Type Bool) 1
    ws <- weights
    ss <- mapM genStreamInfo [1 .. numStreams ws - 1]
    return (s0 : ss)

  where
  genStreamInfo :: Int -> Gen StreamInfo
  genStreamInfo id =
    do
      ws <- weights
      k  <- choose (1, maxBuffSize ws)
      WrapType t <- genType ws
      return
        StreamInfo
          { streamInfoId         = id
          , streamInfoType       = t
          , streamInfoBufferSize = k }

--------------------------------------------------------------------------------

genStream :: [StreamInfo] -> StreamInfo -> Gen Stream
genStream ss
  StreamInfo
    { streamInfoId         = id
    , streamInfoType       = t
    , streamInfoBufferSize = k } =
  do
    xs <- replicateM k (randomFromType t)
    w  <- genExpr ss t
    return
      Stream
        { streamId       = id
        , streamBuffer   = xs
        , streamGuard    = E.Const (typeOf :: Type Bool) True
        , streamExpr     = w
        , streamExprType = t }

--------------------------------------------------------------------------------

genTrigger :: [StreamInfo] -> E.Name -> Gen Trigger
genTrigger ss name =
  do
    w  <- genExpr ss (typeOf :: Type Bool)
    ws <- weights
    i  <- choose (1, maxTrigArgs ws)
    args <- replicateM i genArg
    return
      Trigger
        { triggerName  = name
        , triggerGuard = w
        , triggerArgs  = args }

  where

  genArg :: Gen UExpr
  genArg =
    do
      WrapType t <- genTypeFromStreamInfo's ss
      w <- genExpr ss t
      return
        UExpr
          { uExprExpr = w
          , uExprType = t }

--------------------------------------------------------------------------------

genExpr :: [StreamInfo] -> Type a -> Gen (E.Expr a)
genExpr ss t =
  do
    dp <- depth
    ws <- weights
    if dp >= maxExprDepth ws
      then freq
        [ (constFreq ws, genConst)
        , (dropFreq  ws, genDrop) ]
      else freq
        [ (constFreq ws, genConst)
        , (dropFreq  ws, genDrop)
        , (op1Freq   ws, genOp1)
        , (op2Freq   ws, genOp2)
        , (op3Freq   ws, genOp3) ]

  where
  genConst =
    do
      x <- randomFromType t
      return $ E.Const t x
  genDrop =
    do
      s <- findStreamInfoWithMatchingType
      k <- choose (0, streamInfoBufferSize s - 1)
      return $ E.Drop t (fromIntegral k) (streamInfoId s)

    where
    findStreamInfoWithMatchingType =
      let
        p (StreamInfo _ t1 _) =
          case t =~= t1 of
            Just _ -> True
            _      -> False
      in
        elements (filter p ss)

  genOp1 = incDepth $ case t of
    Bool   -> genOp1Bool ss
    Int8   -> genOp1Num  ss t
    Int16  -> genOp1Num  ss t
    Int32  -> genOp1Num  ss t
    Int64  -> genOp1Num  ss t
    Word8  -> genOp1Num  ss t
    Word16 -> genOp1Num  ss t
    Word32 -> genOp1Num  ss t
    Word64 -> genOp1Num  ss t
    Float  -> genOp1Num  ss t
    Double -> genOp1Num  ss t

  genOp2 = incDepth $ case t of
    Bool    -> oneOf [genOp2Bool ss, genOp2Eq ss, genOp2Ord ss]
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

      intOrWord numWit integralWit = do 
        ws <- weights 
        if divModFreq ws 
          then oneOf $ num ++ [genOp2Integral ss t integralWit ]
          else oneOf num
        where num = [ genOp2Num ss t numWit ]

      floatOrDouble numWit = oneOf
        [ genOp2Num ss t numWit ]

  genOp3 = incDepth (genOp3Mux ss t)

--------------------------------------------------------------------------------

genOp1Bool :: [StreamInfo] -> Gen (E.Expr Bool)
genOp1Bool ss =
  do
    ew <- genExpr ss (typeOf :: Type Bool)
    return $ E.Op1 E.Not ew

genOp1Num :: Num a => [StreamInfo] -> Type a -> Gen (E.Expr a)
genOp1Num ss t =
  do
    ew  <- genExpr ss t
    opw <- elements [E.Abs t, E.Sign t]
    return $ E.Op1 opw ew

genOp2Bool :: [StreamInfo] -> Gen (E.Expr Bool)
genOp2Bool ss =
  do
    ew1 <- genExpr ss (typeOf :: Type Bool)
    ew2 <- genExpr ss (typeOf :: Type Bool)
    opw <- elements [E.And, E.Or]
    return $ E.Op2 opw ew1 ew2

genOp2Eq :: [StreamInfo] -> Gen (E.Expr Bool)
genOp2Eq ss =
  do
    WrapType t <- genTypeFromStreamInfo's ss
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <- elements [E.Eq t, E.Ne t]
    return $ E.Op2 opw ew1 ew2

genOp2Ord :: [StreamInfo] -> Gen (E.Expr Bool)
genOp2Ord ss =
  do
    WrapType t <- genTypeFromStreamInfo's ss
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <- elements
      [ (E.Lt t)
      , (E.Gt t)
      , (E.Le t)
      , (E.Ge t) ]
    return $ E.Op2 opw ew1 ew2

genOp2Num :: [StreamInfo] -> Type a -> NumWit a -> Gen (E.Expr a)
genOp2Num ss t NumWit =
  do
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <-
      elements
        [ (E.Add t)
        , (E.Sub t)
        , (E.Mul t) ]
    return
      $ E.Op2 opw ew1 ew2

genOp2Integral :: [StreamInfo] -> Type a -> IntegralWit a -> Gen (E.Expr a)
genOp2Integral ss t IntegralWit =
  do
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <-
      elements
        [ (E.Div t)
        , (E.Mod t) ]
    return
      $ E.Op2 opw ew1 ew2

genOp3Mux :: [StreamInfo] -> Type a -> Gen (E.Expr a)
genOp3Mux ss t =
  do
    ew1 <- genExpr ss (typeOf :: Type Bool)
    ew2 <- genExpr ss t
    ew3 <- genExpr ss t
    return $ E.Op3 (E.Mux t) ew1 ew2 ew3

data NumWit a = Num a => NumWit

data IntegralWit a = Integral a => IntegralWit
