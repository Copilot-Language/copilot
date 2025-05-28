{-# LANGUAGE ExistentialQuantification #-}

-- | This is a duplicate version of @Copilot.Compile.Bluespec.External@ that is
-- specific to the test suite. Ideally, we would move this into a common library
-- that is shared between both @copilot-bluespec@ and @copilot-c99@ so that we
-- can avoid this duplication. See
-- https://github.com/Copilot-Language/copilot-bluespec/issues/3.
--
-- Represent information about externs needed in the generation of Bluespec
-- code for stream declarations and triggers.
module Test.Copilot.Compile.Bluespec.External
  ( External(..)
  , gatherExts
  ) where

-- External imports
import Data.List (unionBy)

-- External imports: Copilot
import Copilot.Core ( Expr (..), Stream (..), Trigger (..), Type, UExpr (..) )

-- | Representation of external variables.
data External = forall a. External
  { extName :: String
  , extType :: Type a
  }

-- | Collect all external variables from the streams and triggers.
--
-- Although Copilot specifications can contain also properties and theorems,
-- the Bluespec backend currently only generates code for streams and triggers.
gatherExts :: [Stream] -> [Trigger] -> [External]
gatherExts streams triggers = streamsExts `extUnion` triggersExts
  where
    streamsExts  = foldr (extUnion . streamExts) mempty streams
    triggersExts = foldr (extUnion . triggerExts) mempty triggers

    streamExts :: Stream -> [External]
    streamExts (Stream _ _ expr _) = exprExts expr

    triggerExts :: Trigger -> [External]
    triggerExts (Trigger _ guard args) = guardExts `extUnion` argExts
      where
        guardExts = exprExts guard
        argExts   = concatMap uExprExts args

    uExprExts :: UExpr -> [External]
    uExprExts (UExpr _ expr) = exprExts expr

    exprExts :: Expr a -> [External]
    exprExts (Local _ _ _ e1 e2)   = exprExts e1 `extUnion` exprExts e2
    exprExts (ExternVar ty name _) = [External name ty]
    exprExts (Op1 _ e)             = exprExts e
    exprExts (Op2 _ e1 e2)         = exprExts e1 `extUnion` exprExts e2
    exprExts (Op3 _ e1 e2 e3)      = exprExts e1 `extUnion` exprExts e2
                                       `extUnion` exprExts e3
    exprExts (Label _ _ e)         = exprExts e
    exprExts _                     = []

    -- | Union over lists of External, we solely base the equality on the
    -- extName's.
    extUnion :: [External] -> [External] -> [External]
    extUnion = unionBy (\a b -> extName a == extName b)
