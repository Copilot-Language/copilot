module Copilot.Compile.C.Meta where

import Copilot.Core ( Stream (..)
                    , Trigger (..)
                    , UExpr (..)
                    , UType (..)
                    )

data Generator = Generator
  { genBuffName   :: String
  , genValName    :: String
  , genIndexName  :: String
  , genFuncName   :: String
  , genStream     :: Stream
  }

data Guard = Guard
  { guardName     :: String
  , guardCFunc    :: String
  , guardArgs     :: [Argument]
  , guardTrigger  :: Trigger
  }

data Argument = Argument
  { argName :: String
  , argExpr :: UExpr
  }

data External = External
  { exName    :: String
  , exLocName :: String
  , exType    :: UType
  }

{- Abstract program, used to gather information -}
data AProgram = AProgram
  { streams     :: [Stream]
  , generators  :: [Generator]
  , trigguards  :: [Guard]
  , externals   :: [External]
  }
