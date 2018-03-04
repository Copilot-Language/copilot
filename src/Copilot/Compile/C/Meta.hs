module Copilot.Compile.C.Meta where

import Copilot.Core ( Stream (..)
                    , Trigger (..)
                    , UExpr (..)
                    , UType (..)
                    )

data Generator = Generator
  { genBuff   :: String
  , genVal    :: String
  , genIndex  :: String
  , genFunc   :: String
  , genStream :: Stream
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
