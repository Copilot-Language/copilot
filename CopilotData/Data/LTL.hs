module CopilotData.Data.LTL where

import           Data.List ( nub, sort )
import           Prelude   hiding ( mod, (&&), (++), (/=), (<), (==), (>) )
import qualified Prelude   as P

data LTLFormula = LTLTrue
                | LTLProp  Int
                | LTLNot   LTLFormula
                | LTLAnd   LTLFormula LTLFormula
                | LTLUntil LTLFormula LTLFormula
                | LTLNext  LTLFormula
  deriving (Eq, Ord, Show)

ltlEventually :: LTLFormula -> LTLFormula
ltlEventually = LTLUntil LTLTrue

ltlGlobally :: LTLFormula -> LTLFormula
ltlGlobally = LTLNot . ltlEventually . LTLNot

ltlOr :: LTLFormula -> LTLFormula -> LTLFormula
ltlOr x y = LTLNot (LTLNot x `LTLAnd` LTLNot y)

ltlFalse :: LTLFormula
ltlFalse = LTLNot LTLTrue

allProps :: LTLFormula -> [Int]
allProps (LTLTrue)        = []
allProps (LTLProp x)      = [x]
allProps (LTLNot x)       = allProps x
allProps (LTLAnd x1 x2)   = allProps x1 P.++ allProps x2
allProps (LTLUntil x1 x2) = allProps x1 P.++ allProps x2
allProps (LTLNext x)      = allProps x

subformulas :: LTLFormula -> [LTLFormula]
subformulas = nub . sort . subformulas'

subformulas' :: LTLFormula -> [LTLFormula]
subformulas' f@LTLTrue            = [f]
subformulas' f@(LTLProp  _)       = [f]
subformulas' f@(LTLNot   fi)      = f : subformulas' fi
subformulas' f@(LTLAnd   fi1 fi2) = f : (subformulas' fi1 P.++ subformulas' fi2)
subformulas' f@(LTLUntil fi1 fi2) = f : (subformulas' fi1 P.++ subformulas' fi2)
subformulas' f@(LTLNext  fi)      = f : subformulas' fi

-- * Tests

formula1 :: LTLFormula
formula1 = LTLTrue

formula2 :: LTLFormula
formula2 = LTLNot LTLTrue

formula3 :: LTLFormula
formula3 = LTLAnd formula1 formula2

formula4 :: LTLFormula
formula4 = LTLUntil formula1 formula3

formula5 :: LTLFormula
formula5 = LTLNext formula4

formula6 :: LTLFormula
formula6 = LTLProp 55

allFormulas :: [ LTLFormula ]
allFormulas = [ formula1
              , formula2
              , formula3
              , formula4
              , formula5
              , formula6
              ]

test1 :: IO ()
test1 = mapM_ (print . subformulas) allFormulas
