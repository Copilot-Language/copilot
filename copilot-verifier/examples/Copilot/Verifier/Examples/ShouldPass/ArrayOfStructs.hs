{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Copilot.Verifier.Examples.ShouldPass.ArrayOfStructs where

import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Language.Copilot

data S = S { field :: Field "field" Int16 }

instance Struct S where
  typeName _ = "s"
  toValues s = [Value Int16 (field s)]

instance Typed S where
  typeOf = Struct (S (Field 0))

spec :: Spec
spec = trigger "f" ((stream ! 0)#field == 27) [arg stream]
  where
    stream :: Stream (Array 2 S)
    stream = [array [S (Field 27), S (Field 42)]] ++ stream

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
                    mkDefaultCSettings [] "arrayOfStructs" spec'
