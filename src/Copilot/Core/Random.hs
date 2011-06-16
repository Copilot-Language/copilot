--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Core.Random
  ( random ) where

import Control.Monad
import Copilot.Core (WrapExpr (..), WrapOp1 (..), WrapOp2 (..))
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

random :: Weights -> StdGen -> Spec
random = runGen genSpec 0

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
  { streamInfoId   :: E.Id
  , streamInfoType :: Type a }

--------------------------------------------------------------------------------

data WrapType = forall a . (Eq a, Ord a) => WrapType (Type a)

genType :: Gen WrapType
genType = elements
  [ WrapType (typeOf :: Type Bool)
  , WrapType (typeOf :: Type Int8)
  , WrapType (typeOf :: Type Int16)
  , WrapType (typeOf :: Type Int32)
  , WrapType (typeOf :: Type Int64)
  , WrapType (typeOf :: Type Word8)
  , WrapType (typeOf :: Type Word16)
  , WrapType (typeOf :: Type Word32)
  , WrapType (typeOf :: Type Word64)
  , WrapType (typeOf :: Type Float)
  , WrapType (typeOf :: Type Double) ]

genTypeFromStreamInfo's :: [StreamInfo] -> Gen WrapType
genTypeFromStreamInfo's = elements . map (\ (StreamInfo _ t) -> WrapType t)

--------------------------------------------------------------------------------

genStreamInfo's :: Gen [StreamInfo]
genStreamInfo's =
  do
    ws <- weights
    let s0 = StreamInfo 0 (typeOf :: Type Bool)
    ss <- mapM genStreamInfo [1 .. numStreams ws - 1]
    return (s0 : ss)

  where

  genStreamInfo :: Int -> Gen StreamInfo
  genStreamInfo k =
    do
      WrapType t <- genType
      return
        StreamInfo
          { streamInfoId   = k
          , streamInfoType = t }

--------------------------------------------------------------------------------

genStream :: [StreamInfo] -> StreamInfo -> Gen Stream
genStream ss
  StreamInfo
    { streamInfoId   = k
    , streamInfoType = t } =
  do
    ws <- weights
    i  <- choose (1, maxBuffSize ws)
    xs <- replicateM i (randomFromType t)
    w  <- genExpr ss t
    return
      Stream
        { streamId       = k
        , streamBuffer   = xs
        , streamGuard    = Nothing
        , streamExpr     = unWrapExpr w
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
        , triggerGuard = unWrapExpr w
        , triggerArgs  = args }

  where

  genArg :: Gen TriggerArg
  genArg =
    do
      WrapType t <- genTypeFromStreamInfo's ss
      w <- genExpr ss t
      return
        TriggerArg
          { triggerArgExpr = unWrapExpr w
          , triggerArgType = t }

--------------------------------------------------------------------------------

genExpr :: [StreamInfo] -> Type a -> Gen (WrapExpr a)
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
      return $ WrapExpr $ E.const t x

  genDrop =
    do
      s <- findStreamInfoWithMatchingType
      ws <- weights
      k <- choose (0, maxBuffSize ws - 1)
      return $ WrapExpr $ E.drop t (fromIntegral k) (streamInfoId s)

    where

    findStreamInfoWithMatchingType =
      let
        p (StreamInfo _ t1) =
          case t =~= t1 of
            Just _ -> True
            _      -> False
      in
        elements (filter p ss)

  genOp1 = incDepth $ case t of
    Bool   p -> genOp1Bool ss p
    Int8   p -> genOp1Num  ss t (numWit p)
    Int16  p -> genOp1Num  ss t (numWit p)
    Int32  p -> genOp1Num  ss t (numWit p)
    Int64  p -> genOp1Num  ss t (numWit p)
    Word8  p -> genOp1Num  ss t (numWit p)
    Word16 p -> genOp1Num  ss t (numWit p)
    Word32 p -> genOp1Num  ss t (numWit p)
    Word64 p -> genOp1Num  ss t (numWit p)
    Float  p -> genOp1Num  ss t (numWit p)
    Double p -> genOp1Num  ss t (numWit p)

  genOp2 = incDepth $ case t of
    Bool   p -> oneOf [genOp2Bool ss p, genOp2Eq ss p, genOp2Ord ss p]
    Int8   p -> intOrWord p
    Int16  p -> intOrWord p
    Int32  p -> intOrWord p
    Int64  p -> intOrWord p
    Word8  p -> intOrWord p
    Word16 p -> intOrWord p
    Word32 p -> intOrWord p
    Word64 p -> intOrWord p
    Float  p -> floatOrDouble p
    Double p -> floatOrDouble p

    where

      intOrWord p = oneOf
        [ genOp2Num ss t (numWit p)
        , genOp2Integral ss t (integralWit p) ]

      floatOrDouble p = oneOf
        [ genOp2Num ss t (numWit p) ]

  genOp3 = incDepth (genOp3Mux ss t)

--------------------------------------------------------------------------------

genOp1Bool :: [StreamInfo] -> Equal a Bool -> Gen (WrapExpr a)
genOp1Bool ss p =
  do
    ew <- genExpr ss (typeOf :: Type Bool)
    return
      $ coerce (cong (symm p))
      $ WrapExpr
      $ E.op1 E.not
      $ unWrapExpr ew

genOp1Num :: [StreamInfo] -> Type a -> NumWit a -> Gen (WrapExpr a)
genOp1Num ss t NumWit =
  do
    ew  <- genExpr ss t
    opw <- elements [WrapOp1 $ E.abs t, WrapOp1 $ E.sign t]
    return $ WrapExpr $ E.op1 (unWrapOp1 opw) (unWrapExpr ew)

genOp2Bool :: [StreamInfo] -> Equal a Bool -> Gen (WrapExpr a)
genOp2Bool ss p =
  do
    ew1 <- genExpr ss (typeOf :: Type Bool)
    ew2 <- genExpr ss (typeOf :: Type Bool)
    opw <- elements [WrapOp2 E.and, WrapOp2 E.or]
    return
      $ coerce (cong (symm p))
      $ WrapExpr
      $ E.op2 (unWrapOp2 opw) (unWrapExpr ew1) (unWrapExpr ew2)

genOp2Eq :: [StreamInfo] -> Equal a Bool -> Gen (WrapExpr a)
genOp2Eq ss p =
  do
    WrapType t <- genTypeFromStreamInfo's ss
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <- elements [WrapOp2 (E.eq t), WrapOp2 (E.ne t)]
    return
      $ coerce (cong (symm p))
      $ WrapExpr
      $ E.op2 (unWrapOp2 opw) (unWrapExpr ew1) (unWrapExpr ew2)

genOp2Ord :: [StreamInfo] -> Equal a Bool -> Gen (WrapExpr a)
genOp2Ord ss p =
  do
    WrapType t <- genTypeFromStreamInfo's ss
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <- elements
      [ WrapOp2 (E.lt t)
      , WrapOp2 (E.gt t)
      , WrapOp2 (E.le t)
      , WrapOp2 (E.ge t) ]
    return
      $ coerce (cong (symm p))
      $ WrapExpr
      $ E.op2 (unWrapOp2 opw) (unWrapExpr ew1) (unWrapExpr ew2)

genOp2Num :: [StreamInfo] -> Type a -> NumWit a -> Gen (WrapExpr a)
genOp2Num ss t NumWit =
  do
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <-
      elements
        [ WrapOp2 (E.add t)
        , WrapOp2 (E.sub t)
        , WrapOp2 (E.mul t) ]
    return
      $ WrapExpr
      $ E.op2 (unWrapOp2 opw) (unWrapExpr ew1) (unWrapExpr ew2)

genOp2Integral :: [StreamInfo] -> Type a -> IntegralWit a -> Gen (WrapExpr a)
genOp2Integral ss t IntegralWit =
  do
    ew1 <- genExpr ss t
    ew2 <- genExpr ss t
    opw <-
      elements
        [ WrapOp2 (E.div t)
        , WrapOp2 (E.mod t) ]
    return
      $ WrapExpr
      $ E.op2 (unWrapOp2 opw) (unWrapExpr ew1) (unWrapExpr ew2)

genOp3Mux :: [StreamInfo] -> Type a -> Gen (WrapExpr a)
genOp3Mux ss t =
  do
    ew1 <- genExpr ss (typeOf :: Type Bool)
    ew2 <- genExpr ss t
    ew3 <- genExpr ss t
    return $ WrapExpr $ E.op3 (E.mux t)
      (unWrapExpr ew1) (unWrapExpr ew2) (unWrapExpr ew3)

--------------------------------------------------------------------------------

data NumWit a = Num a => NumWit

numWit :: Num b => Equal a b -> NumWit a
numWit p = coerce2 (symm p) NumWit

--------------------------------------------------------------------------------

data IntegralWit a = Integral a => IntegralWit

integralWit :: Integral b => Equal a b -> IntegralWit a
integralWit p = coerce2 (symm p) IntegralWit

--------------------------------------------------------------------------------