{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

-- | A representation for structured expression trees, with support for pretty
-- printing.
module Copilot.Theorem.Misc.SExpr where

import Text.PrettyPrint.HughesPJ as PP hiding (char, Str)

-- | A structured expression is either an atom, or a sequence of expressions,
-- where the first in the sequence denotes the tag or label of the tree.
data SExpr a = Atom a
             | List [SExpr a]

-- | Empty string expression.
blank :: SExpr String
blank = Atom ""

-- | Atomic expression constructor.
atom :: a -> SExpr a
atom = Atom                 -- s

-- | Empty expression (empty list).
unit :: SExpr a
unit = List []              -- ()

-- | Single expression.
singleton :: a -> SExpr a
singleton a  = List [Atom a]        -- (s)

-- | Sequence of expressions.
list :: [SExpr a] -> SExpr a
list = List                 -- (ss)

-- | Sequence of expressions with a root or main note, and a series of
-- additional expressions or arguments.
node :: a -> [SExpr a] -> SExpr a
node a l = List (Atom a : l)    -- (s ss)

-- A straightforward string representation for 'SExpr's of Strings that
-- parenthesizes lists of expressions.
instance Show (SExpr String) where
  show = PP.render . show'
    where
      show' (Atom s) = text s
      show' (List ts) = parens . hsep . map show' $ ts

-- More advanced printing with some basic indentation

-- | Indent by a given number.
indent :: Doc -> Doc
indent = nest 1

-- | Pretty print a structured expression as a String.
toString :: (SExpr a -> Bool)  -- ^ True if an expression should be indented.
         -> (a -> String)      -- ^ Pretty print the value inside as 'SExpr'.
         -> SExpr a            -- ^ Root of 'SExpr' tree.
         -> String
toString shouldIndent printAtom expr =
  PP.render (toDoc shouldIndent printAtom expr)

-- | Pretty print a structured expression as a 'Doc', or set of layouts.
toDoc :: (SExpr a -> Bool)  -- ^ True if an expression should be indented.
      -> (a -> String)      -- ^ Pretty print the value inside as 'SExpr'.
      -> SExpr a            -- ^ Root of 'SExpr' tree.
      -> Doc
toDoc shouldIndent printAtom expr = case expr of
  Atom a  -> text (printAtom a)
  List l  -> parens (foldl renderItem empty l)

  where
    renderItem doc s
      | shouldIndent s =
        doc $$ indent (toDoc shouldIndent printAtom s)
      | otherwise =
        doc <+> toDoc shouldIndent printAtom s
