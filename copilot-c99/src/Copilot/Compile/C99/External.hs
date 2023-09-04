{-# LANGUAGE ExistentialQuantification #-}

-- | Represent information about externs needed in the generation of C99 code
-- for stream declarations and triggers.
module Copilot.Compile.C99.External where

import Data.List  (unionBy)

import Copilot.Core
import Copilot.Compile.C99.Util

-- | Representation of external variables.
data External = forall a. External
  { extName    :: String
  , extCpyName :: String
  , extType    :: Type a
  }

-- | Union over lists of External, we solely base the equality on the
-- extName's.
extUnion :: [External] -> [External] -> [External]
extUnion = unionBy (\a b -> extName a == extName b)

-- | Collect all external variables from the streams and triggers.
--
-- Although Copilot specifications can contain also properties and theorems,
-- the C99 backend currently only generates code for streams and triggers.
gatherExts :: [Stream] -> [Trigger] -> [External]
gatherExts streams triggers = streamsExts `extUnion` triggersExts
  where
    streamsExts  = foldr extUnion mempty $ map streamExts streams
    triggersExts = foldr extUnion mempty $ map triggerExts triggers

    streamExts :: Stream -> [External]
    streamExts (Stream _ _ expr _) = exprExts expr

    triggerExts :: Trigger -> [External]
    triggerExts (Trigger _ guard args) = guardExts `extUnion` argExts
      where
        guardExts = exprExts guard
        argExts   = concat $ map uExprExts args

    uExprExts :: UExpr -> [External]
    uExprExts (UExpr _ expr) = exprExts expr

    exprExts :: Expr a -> [External]
    exprExts expr = let rec = exprExts in case expr of
      Local _ _ _ e1 e2   -> rec e1 `extUnion` rec e2
      ExternVar ty name _ -> [External name (exCpyName name) ty]
      Op1 _ e             -> rec e
      Op2 _ e1 e2         -> rec e1 `extUnion` rec e2
      Op3 _ e1 e2 e3      -> rec e1 `extUnion` rec e2 `extUnion` rec e3
      Label _ _ e         -> rec e
      _                   -> []
