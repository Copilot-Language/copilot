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

stepACSL :: AProgram -> Doc
stepACSL ap = text "/*@" $$ nest 4 (vcat spec) $$ text "*/" where
  spec = requires ++ assigns ++ ensures
  gens = generators ap

  requires = map validindex gens
  assigns = map assignsrule (vals ++ buffers ++ indices) where
    vals    = map genValName gens
    buffers = map (\g -> buffidx (genBuffName g) (genIndexName g)) gens
    indices = map genIndexName gens
  ensures  = map arraysame gens

  {- Simple indexing of a buffer -}
  buffidx :: String -> String -> String
  buffidx buff idx = buff ++ "[" ++ idx ++ "]"

  {- Create an assigns rule from string -}
  assignsrule :: String -> Doc
  assignsrule var = text "assigns  " <> text var <> semi

  {- Ensures that all elements of array stay equal,
   - except for i == \old(s*_idx) -}
  arraysame :: Generator -> Doc
  arraysame g@(Generator _ _ _ _ (Stream _ b _ _)) =
    text "ensures  \\forall int i; 0 <= i < " <> int max <>
        text "&& i !=" <+> oldidx <+> text "==>" $$
        nest 4 (buff <+> text "==" <+> oldbuff <> semi) where
      idx     = text (genIndexName g)
      buff    = text (genBuffName g) <> brackets (char 'i')
      oldidx  = text "\\old(" <> idx <> text ")"
      oldbuff = text "\\old(" <> buff <> text ")"
      max     = length b

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
streamACSL ap e = text "/*@" $$ nest 4 (vcat spec) $$ text "*/" where
  spec =  requires ++ indices ++
          [ text "assigns  \\nothing;"
          , text "ensures  \\result == " <> result <> semi
          ]
  gens = generators ap
  deps = dependencies e
  requires = map validrange (join deps gens)
  indices  = map validindex (join deps gens)
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
validrange g = text "requires \\valid" <+> parens (var <> char '+' <> range) <> semi where
  var     = text $ genBuffName g
  range   = parens (int 0 <> text ".." <> int (bufflen (genStream g) - 1))
  bufflen (Stream _ buff _ _) = length buff

{- Check if the index of the generator is valid -}
validindex :: Generator -> Doc
validindex g@(Generator _ _ _ _ (Stream _ b _ _)) =
  text "requires" <+> int 0 <+> le <+> idx <+> lt <+> int max <> semi where
    idx = text $ genIndexName g
    le  = text "<="
    lt  = text "<"
    max = length b

{- Joins a list of id's with their generators -}
join :: [Id] -> [Generator] -> [Generator]
join ids gens = [ gen | i <- ids, gen <- gens, (streamId $ genStream gen) == i ]
