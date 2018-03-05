{-# LANGUAGE GADTs #-}

module Copilot.Compile.ACSL.CodeGen where

import Copilot.Core ( Stream (..)
                    , Expr (..)
                    , UExpr (..)
                    , Trigger (..)
                    , Id
                    )

import Copilot.Compile.C.Meta
import Copilot.Compile.ACSL.Expr

import Data.List
import Data.Maybe (fromJust)
import Text.PrettyPrint

streamgenACSL :: AProgram -> Generator -> Doc
streamgenACSL ap (Generator _ _ _ _ (Stream _ _ e _)) =
  streamACSL ap e

guardgenACSL :: AProgram -> Guard -> Doc
guardgenACSL ap g = streamACSL ap (triggerGuard $ guardTrigger g)

arggenACSL :: AProgram -> Argument -> Doc
arggenACSL ap (Argument _ (UExpr _ e)) = streamACSL ap e

{- Write ACSL specification with:
 - * List of requires for every external streambuffer
 - * assigns \nothing
 - * ensures \result == evaluation of expression
 - This function is only used by more specific *genACSL functions -}
streamACSL :: AProgram -> Expr a -> Doc
streamACSL ap e = foldr ($+$) empty (
  [ text "/*@" ]
  ++ requires ++
  [ text " *  assigns \\nothing;"
  , text " *  ensures \\result == " <> result
  , text " */"
  ] ) where
    gens = generators ap
    requires = map validrange (join (dependencies e) gens)
    result = exprACSL ap e

{- Streams where the given stream depends on -}
dependencies :: Expr a -> [Id]
dependencies e = case e of
  Drop _ _ id       -> [id]
  Local _ _ _ e1 e2 -> dependencies e1 `union` dependencies e2
  Op1 _ e           -> dependencies e
  Op2 _ e1 e2       -> dependencies e1 `union` dependencies e2
  Op3 _ e1 e2 e3    -> dependencies e1 `union` dependencies e2 `union` dependencies e3
  _                 -> []


{- Returns a 'requires \valid <range>' -}
validrange :: Generator -> Doc
validrange g = text " *  requires \\valid " <> parens (var <> char '+' <> range) <> char ';' where
  var     = text $ genBuffName g
  range   = parens (int 0 <> text ".." <> int (bufflen (genStream g) - 1))
  bufflen (Stream _ buff _ _) = length buff

{- Joins a list of id's with their generators -}
join :: [Id] -> [Generator] -> [Generator]
join ids gens = [ gen | i <- ids, gen <- gens, (streamId $ genStream gen) == i ]
