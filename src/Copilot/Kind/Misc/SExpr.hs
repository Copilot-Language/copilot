--------------------------------------------------------------------------------

module Copilot.Kind.Misc.SExpr where

import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ as PP hiding (char, Str)
import Control.Applicative hiding ((<|>), empty)

import Control.Monad

--------------------------------------------------------------------------------

data SExpr a = Atom a
             | List [SExpr a]

atom         = Atom                 -- s
unit         = List []              -- ()
singleton a  = List [Atom a]        -- (s)
list         = List                 -- (ss)
node a l     = List (Atom a : l)    -- (s ss)

--------------------------------------------------------------------------------

-- A straightforward string representation

instance Show (SExpr String) where
  show = PP.render . show'
    where
      show' (Atom s) = text s
      show' (List ts) = parens . hsep . map show' $ ts


-- More advanced printing with some basic indentation

indent = nest 1

toString :: (SExpr a -> Bool) -> (a -> String) -> SExpr a -> String
toString shouldIndent printAtom expr =
  PP.render (toDoc shouldIndent printAtom expr)

toDoc :: (SExpr a -> Bool) -> (a -> String) -> SExpr a -> Doc
toDoc shouldIndent printAtom expr = case expr of
  Atom a  -> text (printAtom a)
  List l  -> parens (foldl renderItem empty l)

  where renderItem doc s
          | shouldIndent s =
            doc $$ indent (toDoc shouldIndent printAtom s)
          | otherwise =
            doc <+> toDoc shouldIndent printAtom s

--------------------------------------------------------------------------------

parser :: GenParser Char st (SExpr String)
parser =
  choice [try unitP, nodeP, leafP]

  where symbol     = oneOf "!#$%&|*+-/:<=>?@^_~."        
        lonelyStr  = many1 (alphaNum <|> symbol)

        unitP      = string "()" >> return unit

        leafP      = atom <$> lonelyStr
          
        nodeP      = do void $ char '('
                        spaces
                        st <- sepBy parser spaces
                        spaces
                        void $ char ')'
                        return $ List st


parseSExpr :: String -> Maybe (SExpr String)
parseSExpr str = case parse parser "" str of
  Left s -> error (show s) -- Nothing
  Right t -> Just t

--------------------------------------------------------------------------------
