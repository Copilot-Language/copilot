module Copilot.Kind.Cnub.PrettyPrint (prettyPrint) where

import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators
import Copilot.Kind.Cnub.Spec
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as Map

prettyPrint :: Spec -> String
prettyPrint = render . ppSpec

indent = nest 4
emptyLine = text ""

ppSpec :: Spec -> Doc
ppSpec 
  ( Spec
  { specStreams
  , specProperties
  , specObservers }) =
  
     text "STREAMS" $$ emptyLine
  $$ ppMap ppStream specStreams
  $$ text "PROPERTIES"
  $$ ppMap ppProperty specProperties
  $$ text "OBSERVERS"
  $$ ppMap ppObserver specObservers
  where
    ppMap f m = 
      indent (foldr (($$) . f) empty (Map.toList m))


ppStream :: (StreamId, Stream) -> Doc
ppStream (id, Stream {streamBuffer, streamType, streamExpr}) =
  parens (ppType streamType) <+> text id <+> text "=" 
  <+> (brackets . hsep . punctuate (comma <> space)) 
        (map (ppExpr . Const streamType) streamBuffer)
  <+> text "++"
  <+> ppExpr streamExpr
  
ppExpr :: Expr a -> Doc
ppExpr (Const Integer v) = text . show $ v
ppExpr (Const Bool    v) = text . show $ v
ppExpr (Const Real    v) = text . show $ v

ppExpr (Op1 _ op e) = ppOp1 op <+> ppExpr e

ppExpr (Op2 _ op e1 e2) =
  ppExpr e1 <+> ppOp2 op <+> ppExpr e2

ppExpr (Ite _ c e1 e2) =
  text "if" <+> ppExpr c
  <+> text "then" <+> ppExpr e1
  <+> text "else" <+> ppExpr e2
  
ppExpr (Drop _ 0 id) = text id
ppExpr (Drop _ k id) = text "drop" <+> int k <+> text id
  
ppExpr (Unint _ id []) = text "'" <+> text id
ppExpr (Unint _ id args) = 
  text "'" <+> text id 
  <> parens (hsep . punctuate (comma <> space) $ map ppUExpr args)

ppType :: Type a -> Doc
ppType = text . show

ppOp1 :: Op1 a b -> Doc
ppOp1 = text . show

ppOp2 :: Op2 a b c -> Doc
ppOp2 = text . show

ppUExpr :: U Expr -> Doc
ppUExpr (U e) = ppExpr e

ppProperty :: (PropId, StreamId) -> Doc
ppProperty (id, s) = text id <+> colon <+> text s

ppObserver :: (ObserverId, StreamId) -> Doc
ppObserver (id, s) = text id <+> colon <+> text s 
