module Copilot.Compile.C.Util where

import Copilot.Compile.C.Translation

import Copilot.Core

import Language.C99.AST
import Language.C99.Util

{- TODO: move these functions -}
ptrdeclr :: String -> Maybe Init -> InitDeclr
ptrdeclr n = _declr n (Just (PBase Nothing))

declr :: String -> Maybe Init -> InitDeclr
declr n = _declr n Nothing

_declr :: String -> Maybe Ptr -> Maybe Init -> InitDeclr
_declr n ptr Nothing  = IDDeclr (Dr ptr (DDIdent $ ident n))
_declr n ptr (Just i) = IDInit  (Dr ptr (DDIdent $ ident n)) i

arrdeclr :: String -> [Int] -> Init -> InitDeclr
arrdeclr name (l:ls) init = IDInit (Dr Nothing dds) init where
  dds = foldl cons base ls
  base = DDArray1 (DDIdent $ ident name) Nothing (len l)
  cons xs l = DDArray1 xs Nothing (len l)
  len n = Just $ constty Int32 (fromIntegral n)

vardef :: DeclnSpecs -> [InitDeclr] -> Decln
vardef ds []       = Dn ds Nothing
vardef ds (i:ids)  = Dn ds dl where
  dl = Just $ foldl IDLCons (IDLBase i) ids
