-- Dynamic StableName's without phantom type.

module Language.Copilot.Interface.DynStableName
  ( StableName
  , makeStableName
  , hashStableName
  ) where

import qualified System.Mem.StableName as S
import Unsafe.Coerce (unsafeCoerce)

newtype StableName = StableName (S.StableName ())

instance Eq StableName where
  (StableName x) == (StableName y) = x == y

makeStableName :: a -> IO StableName
makeStableName a = do
  sn <- S.makeStableName a
  return $ StableName (unsafeCoerce sn)

hashStableName :: StableName -> Int
hashStableName (StableName sn) = S.hashStableName sn
