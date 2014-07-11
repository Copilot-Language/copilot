--------------------------------------------------------------------------------

module Copilot.Kind.Kind2.Output (isPropertyValid) where

import Text.XML.Light hiding (findChild)

--------------------------------------------------------------------------------

simpleName s = QName s Nothing Nothing

isPropertyValid :: String -> String -> Maybe Bool
isPropertyValid prop s = do
  root <- parseXMLDoc s
  let (section : _)  = filterChildren rightElement root
  let (answTag : _) = findChildren (simpleName "Answer") section
  let (answ : _) = onlyText (elContent answTag)
  case cdData answ of
    "valid"   -> return True
    "invalid" -> return False
    _         -> Nothing  
  
  where rightElement elt = 
          qName (elName elt) == "Property"
          && lookupAttr (simpleName "name") (elAttribs elt) 
             == Just prop

--------------------------------------------------------------------------------
