--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe       #-}

-- | Parse output of Kind2.
module Copilot.Theorem.Kind2.Output (parseOutput) where

import Text.XML.Light       hiding (findChild)
import Copilot.Theorem.Prove  as P
import Data.Maybe           (fromJust)

import qualified Copilot.Theorem.Misc.Error as Err

--------------------------------------------------------------------------------

simpleName s = QName s Nothing Nothing

-- | Parse output of Kind2.
parseOutput :: String    -- ^ Property whose validity is being checked.
            -> String    -- ^ XML output of Kind2
            -> P.Output
parseOutput prop xml = fromJust $ do
  root <- parseXMLDoc xml
  case findAnswer . findPropTag $ root of
    "valid"   -> return (Output Valid   [])
    "invalid" -> return (Output Invalid [])
    s         -> err $ "Unrecognized status : " ++ s

  where

    searchForRuntimeError = undefined

    findPropTag root =
      let rightElement elt =
            qName (elName elt) == "Property"
            && lookupAttr (simpleName "name") (elAttribs elt)
                == Just prop
      in case filterChildren rightElement root of
           tag : _ -> tag
           _ -> err $ "Tag for property " ++ prop ++ " not found"

    findAnswer tag =
      case findChildren (simpleName "Answer") tag of
        answTag : _ ->
          case onlyText (elContent answTag) of
            answ : _ -> cdData answ
            _ -> err "Invalid 'Answer' attribute"
        _ -> err "Attribute 'Answer' not found"

    err :: forall a . String -> a
    err msg = Err.fatal $
      "Parse error while reading the Kind2 XML output : \n"
      ++ msg ++ "\n\n" ++ xml

--------------------------------------------------------------------------------
