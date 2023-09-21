-- {-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE GADTs #-}
import Prelude hiding (True, False, init)
import qualified Prelude

-- import qualified Language.Copilot
-- import qualified Copilot.Interpret
--- import Copilot.PrettyPrint

-- import Data.List
-- import Data.String
-- import Data.Char
-- import System.IO
import Debug.Trace

data Truth = T | F | PT | PF
  deriving (Eq, Ord, Read)

instance Show Truth where
  show T  = "T"
  show F  = "F"
  show PT  = "PT"
  show PF  = "PF"

data AbsVar where
  Var :: String -> AbsVar
  Add :: AbsVar -> Int -> AbsVar
  -- Minus :: AbsVar -> Int -> AbsVar
  deriving (Eq, Ord, Read)

instance Show AbsVar where
  -- An stream with `label` at `index` will return `future` when the value is not ready yet
  show (Var name) = name
  show (Add var val) | val >= 0 = show var ++ "+" ++ show val
  show (Add var val) | val < 0 = show var ++ show val
  -- show (Minus var val) = show var ++ "-" ++ show val

type Label = Int

data TFormula where
  -- labeled variables/streams
  Stream :: Label -> TFormula

  -- standard logical operators
  LAnd :: TFormula -> TFormula -> TFormula
  LOr :: TFormula -> TFormula -> TFormula
  -- Implies :: TFormula -> TFormula -> TFormula
  LNot :: TFormula -> TFormula

  -- FTLTL operators, unbounded
  Globally :: TFormula -> TFormula         -- globally/box
  Finally :: TFormula -> TFormula          -- will it be true in any future time step
  Next :: TFormula -> TFormula             -- is it true in the next time step
  Until :: TFormula -> TFormula -> TFormula -- has the first stream held until the second stream has been true

  Constant :: Truth -> TFormula
  deriving (Eq, Ord, Read)

instance Show TFormula where
  -- questions: what is zeta? should I implement equiv, xor, etc?
  show (Stream label) = "s_" ++ show label ++ ""

  show (LAnd f1 f2) = "(" ++ show f1 ++ " and " ++ show f2 ++ ")"
  show (LOr f1 f2) = "(" ++ show f1 ++ " or " ++ show f2 ++ ")"
  -- show (Implies f1 f2) = "IMPLIES_MTL(" ++ show f1 ++ "," ++ show f2 ++ ")"
  show (LNot f1) = "(not" ++ show f1 ++ ")"

  show (Globally f1) = "G(" ++ show f1 ++ ")"
  show (Finally f1) = "F(" ++ show f1 ++ ")"
  show (Next f1) = "X(" ++ show f1 ++ ")"
  show (Until f1 f2) = "((" ++ show f1 ++ ")U(" ++ show f2 ++ "))"
  show (Constant val) = show val

type FreshVar = Int

data MFormula where
  IndexedStream :: Label -> AbsVar -> MFormula
  Forall :: AbsVar -> AbsVar -> AbsVar -> MFormula -> MFormula
  Exists :: AbsVar -> AbsVar -> AbsVar -> MFormula -> MFormula

  And :: MFormula -> MFormula -> MFormula
  Or :: MFormula -> MFormula -> MFormula
  Not :: MFormula -> MFormula

  Const :: Truth -> MFormula

  -- casting
  Pt :: MFormula -> MFormula
  Pf :: MFormula -> MFormula 
  P :: MFormula -> MFormula

  deriving (Eq, Ord, Read)

instance Show MFormula where
  -- questions: what is zeta? should I implement equiv, xor, etc?
  -- An stream with `label` at `index` will return `future` when the value is not ready yet
  show (IndexedStream label index) = "s_" ++ show label ++ "[" ++ show index ++ "]"
  show (Forall var lb ub formula) = "forall " ++ show var ++ " in [" ++ show lb ++ ".." ++ show ub ++ "], " ++ show formula
  show (Exists var lb ub formula) = "exists " ++ show var ++ " in [" ++ show lb ++ ".." ++ show ub ++ "], " ++ show formula

  show (And f1 f2) = "(" ++ show f1 ++ ") and (" ++ show f2 ++ ")"
  show (Or f1 f2) = "(" ++ show f1 ++ ") or (" ++ show f2 ++ ")"
  -- show (Implies f1 f2) = "IMPLIES_MTL(" ++ show f1 ++ "," ++ show f2 ++ ")"
  show (Not f1) = "(not" ++ show f1 ++ ")"
  show (Const val)  = show val

  -- casting
  show (Pt f1) = "Pt(" ++ show f1 ++ ")"
  show (Pf f1) = "Pf(" ++ show f1 ++ ")"
  show (P f1) = "P(" ++ show f1 ++ ")"

strings :: [String]
strings = [ c : s | s <- "": strings, c <- ['a'..'z'] ]

gen :: FreshVar -> AbsVar
gen fresh = Var (strings !! fresh)

-- standard evaluation semantics
seval :: FreshVar -> TFormula -> AbsVar -> AbsVar -> MFormula
-- for the Stream case, we assume that n < m
seval _   (Stream label) n m = IndexedStream label n
seval fresh (LAnd f1 f2) n m = And (seval fresh f1 n m ) (seval fresh f2 n m)
seval fresh (LOr f1 f2) n m = Or (seval fresh f1 n m) (seval fresh f2 n m)
seval fresh (LNot f1) n m = Not (seval fresh f1 n m)
-- where the *magic* happens
seval fresh (Globally f1) n m = Forall i n (Add m (-1)) (seval (fresh+1) f1 i m) where i = gen fresh
seval fresh (Finally f1) n m = Exists i n (Add m (-1)) (seval (fresh+1) f1 i m) where i = gen fresh
seval fresh (Until f1 f2) n m = Exists i n (Add m (-1)) (And (Forall j n (Add i (-1)) (f1')) f2')
  where 
    i = gen fresh
    j = gen (fresh+1)
    f1' = seval (fresh+2) f1 j m
    f2' = seval (fresh+2) f2 i m
seval fresh (Next f1) n m = Pf (seval fresh f1 (Add n 1) m)
seval _ (Constant n) _ _ = Const n


sevalTestWrap :: TFormula -> String
sevalTestWrap expr = "evaluating:\n    " ++ show expr ++ "\nat " ++ show n ++ " till " ++ show m ++ " obtains: \n    " ++ show (seval fresh expr n m)
  where
    n = (gen 0)
    m = (gen 1)
    fresh = 2

sevalTest1 :: String
sevalTest1 = sevalTestWrap (Globally (Stream 0))
sevalTest2 :: String
sevalTest2 = sevalTestWrap (LAnd (Globally (Until (Stream 0) (Stream 1))) (Stream 0))


compileEval :: TFormula -> IO ()
compileEval expr = do
  help "compiling:"
  putStrLn (show expr)
  help $ "at " ++ show n ++ " till " ++ show m ++ " obtains:"
  putStrLn (show init)
  help "after constantFolding 1:"
  putStrLn (show folded)
  help "after expanding:"
  putStrLn (show expanded)
  help "after constantFolding 2:"
  putStrLn (show $ constantFolding expanded)
  help "after primitive licm:"
  putStrLn (show $ licm $ constantFolding expanded)
  help $ "rec call (i.e. evaluating " ++ show n ++ " till " ++ show m' ++ ") is:"
  putStrLn (show rec)
  help "after constantFolding (rec call)"
  putStrLn (show $ constantFolding rec)
  putStrLn ""
  where
    n = (gen 0)
    m' = (Add (gen 1) 1)
    m = (Add m' 1)
    fresh = 2
    init = (seval fresh expr n m)
    folded = constantFolding init
    expanded = expand folded
    rec = seval fresh expr n m'
    help msg = do 
      putStrLn msg
      putStr "    "

compileEval' :: TFormula -> IO ()
compileEval' expr = do
  help "compiling:"
  putStrLn (show expr)
  help $ "at " ++ show m' ++ " till " ++ show m ++ " obtains:"
  putStrLn (show init)
  help "after expanding:"
  putStrLn (show expanded)
  help "after constantFolding:"
  putStrLn (show $ constantFolding expanded)
  help "after primitive licm:"
  putStrLn (show $ licm $ constantFolding expanded)
  -- help $ "rec call (i.e. evaluating " ++ show n ++ " till " ++ show m' ++ ") is:"
  -- putStrLn (show rec)
  putStrLn ""
  where
    n = (gen 0)
    m' = (Add (gen 1) 1)
    m = (Add m' 1)
    fresh = 2
    init = (seval fresh expr m' m)
    expanded = expand init
    -- rec = show (seval fresh expr m m')
    help msg = do 
      putStrLn msg
      putStr "    "
       

compileEvalTest1 :: IO ()
compileEvalTest1 = compileEval (Globally (Stream 0))
compileEvalTest2 :: IO ()
compileEvalTest2 = compileEval (LAnd (Globally (Until (Stream 0) (Stream 1))) (Stream 0))
compileEvalTest3 :: IO ()
compileEvalTest3 = compileEval (Finally (Globally (Stream 0)))
compileEvalTest4 :: IO ()
compileEvalTest4 = compileEval (Globally (Until (Stream 0) (Stream 1)))
compileEvalTest5 :: IO ()
compileEvalTest5 = compileEval (Globally (Finally (Stream 0)))

main :: IO ()
main = do
  compileEvalTest1
  compileEvalTest2
  compileEvalTest3
  compileEvalTest4
  compileEvalTest5
  compileEval' (Until (Stream 0) (Stream 1))

-- evaluation for recusive monitor construction
subVar :: AbsVar -> AbsVar -> AbsVar -> AbsVar -- non-recurisve, maybe it should be
subVar val match sub | val == match = sub
subVar (Add var val) match sub | val > 1 = Add (subVar (Add var (val-1)) match sub) 1
subVar (Add var val) match sub | val <= 1 = Add (subVar var match sub) val
-- subVar (Minus var val) match sub = Minus (subVar var match sub) val
subVar val match sub | otherwise = val

mapVar :: MFormula -> AbsVar -> AbsVar -> MFormula
mapVar (IndexedStream label index) match sub = IndexedStream label (subVar index match sub)
mapVar (Forall i n m form)         match sub = Forall i (subVar n match sub) (subVar m match sub) (mapVar form match sub)
mapVar (Exists i n m form)         match sub = Exists i (subVar n match sub) (subVar m match sub) (mapVar form match sub)
-- mapVar (And f1 f2)                 match sub = And (mapVar f1 match sub) (mapVar f2 match sub)
-- mapVar (Or f1 f2)                  match sub = Or (mapVar f1 match sub) (mapVar f2 match sub)
-- mapVar (Not f)                     match sub = Not (mapVar f match sub)
mapVar formula                     match sub = thread formula (\x -> mapVar x match sub)
-- mapVar formula match substitute = _

optimize :: MFormula -> MFormula
optimize = licm . constantFolding 

-- recursively apply a func to each node of an AST
thread :: MFormula -> (MFormula -> MFormula) -> MFormula
-- not sure if these two cases are necessary, tbh
thread (Forall i n m f1) func = Forall i n m (func f1)
thread (Exists i n m f1) func = Exists i n m (func f1)
thread (And f1 f2) func = And (func f1) (func f2)
thread (Or f1 f2) func = Or (func f1) (func f2)
thread (Not f1) func = Not (func f1)
thread (Pt f1) func = Pt (func f1)
thread (Pf f1) func = Pf (func f1)
thread (P f1) func = P (func f1)
thread f1 _ = f1

-- this will return true if two vars do not reference each other and false otherwise
-- basically, if the vars are independent of each other, then this will return true
-- example: b and b+1 are not independent
-- example: b+1 and b+2 are not independent
varsInd :: AbsVar -> AbsVar -> Bool
varsInd (Var n) (Var m) = not (n == m) -- base case
varsInd v@(Var _) (Add var1 _) = varsInd v var1
-- varsInd v@(Var _) (Minus var1 _) = varsInd v var1
varsInd (Add var1 _) v@(Var _) = varsInd v var1
-- varsInd (Minus var1 _) v@(Var _) = varsInd v var1
-- varsInd (Minus var1 _) (Minus var2 _) = varsInd var1 var2
-- varsInd (Minus var1 _) (Add var2 _) = varsInd var1 var2
varsInd (Add var1 _) (Add var2 _) = varsInd var1 var2
-- varsInd (Add var1 _) (Minus var2 _) = varsInd var1 var2

-- first compiler pass
expand :: MFormula -> MFormula
expand (Forall i n m@(Add m' 1) f1) | varsInd n m = And (expand $ Forall i n m' (expand f1)) (Pt $ mapVar f1 i m)
expand (Exists i n m@(Add m' 1) f1) | varsInd n m = Or  (expand $ Exists i n m' (expand f1)) (Pf $ mapVar f1 i m)
-- expand (Forall i n m@(Add m' 1) f1) | varsInd n m = And (expand $ Forall i n m' (expand f1)) (trace ("------{" ++ show f1 ++ " / " ++ show i ++ " / " ++ show m ++ "}------") $ mapVar f1 i m)
-- expand (Exists i n m@(Add m' 1) f1) | varsInd n m = Or  (expand $ Exists i n m' (expand f1)) (trace ("------{" ++ show f1 ++ " / " ++ show i ++ " / " ++ show m ++ "}------") $ mapVar f1 i m)
expand f1 = thread f1 expand

-- reduce (a + 1 + 1) to (a + 2) and (a + 1 -1) to (a + 0);
normAbsVar :: AbsVar -> AbsVar
normAbsVar v@(Var _) = v
normAbsVar (Add var 0) = normAbsVar var
normAbsVar v@(Add (Var _) val) = v
normAbsVar (Add (Add var val1) val2) = normAbsVar (Add var (val1 + val2))

-- if we have a forall i in [n+1...n+1], sub all occurences of i with n+1
-- we also normalize n+1-1 to n, etc.
constantFolding :: MFormula -> MFormula
constantFolding (Forall i n m f1) | (normAbsVar n) == (normAbsVar m) = constantFolding $ Pt (mapVar f1 i (normAbsVar n)) -- not sure if the pt is sound
constantFolding (Forall i n m f1) | otherwise = Forall i (normAbsVar n) (normAbsVar m) (constantFolding f1)
constantFolding (Exists i n m f1) | (normAbsVar n) == (normAbsVar m) = constantFolding $ Pf (mapVar f1 i (normAbsVar n)) -- not sure if the pf is sound
constantFolding (Exists i n m f1) | otherwise = Exists i (normAbsVar n) (normAbsVar m) (constantFolding f1)
constantFolding (IndexedStream label var) = IndexedStream label (normAbsVar var)
constantFolding (Pt (Pf f1)) = constantFolding (P f1)
constantFolding (Pf (Pt f1)) = constantFolding (P f1)
constantFolding (Pf (Pf f1)) = constantFolding (Pf f1)
constantFolding (Pt (Pt f1)) = constantFolding (Pt f1)
constantFolding (Pt (Forall i n m f1)) = constantFolding (Forall i n m f1)
constantFolding (Pf (Forall i n m f1)) = constantFolding (Forall i n m (Pf f1))
constantFolding (P (Forall i n m f1)) = constantFolding (Forall i n m (P f1))
constantFolding (Pf (Exists i n m f1)) = constantFolding (Exists i n m f1)
constantFolding (Pt (Exists i n m f1)) = constantFolding (Exists i n m (Pt f1))
constantFolding (P (Exists i n m f1)) = constantFolding (Exists i n m (P f1))
constantFolding (Pf (And f1 s@(IndexedStream _ _))) = constantFolding (And f1 (Pf s))
constantFolding (Pf (And f1 f2@(Pt _))) = constantFolding (And f1 (Pf f2))
constantFolding (Pf (And f1 f2@(Pf _))) = constantFolding (And f1 (Pf f2))
constantFolding (Pf (And f1 f2@(P _))) = constantFolding (And f1 (Pf f2))
constantFolding (Pt (Or f1 s@(IndexedStream _ _))) = constantFolding (Or f1 (Pt s))
constantFolding (Pt (Or f1 f2@(Pt _))) = constantFolding (Or f1 (Pt f2))
constantFolding (Pt (Or f1 f2@(Pf _))) = constantFolding (Or f1 (Pt f2))
constantFolding (Pt (Or f1 f2@(P _))) = constantFolding (Or f1 (Pt f2)) -- todo: add rules for negation
constantFolding (P (Or f1 f2)) = constantFolding (Or (P f1) (P f2))
constantFolding (P (And f1 f2)) = constantFolding (And (P f1) (P f2))
constantFolding (P (Not f1)) = constantFolding (Not (P f1)) -- todo: double check this
constantFolding f1 = thread f1 constantFolding

-- returns true if AbsVar is not referenced in MFormula
depIndAnalysis :: MFormula -> AbsVar -> Bool
depIndAnalysis (IndexedStream _ index) var = varsInd index var
depIndAnalysis (Forall _ n m formula) var = (varsInd n var) && (varsInd m var) && depIndAnalysis formula var
depIndAnalysis (Exists _ n m formula) var = (varsInd n var) && (varsInd m var) && depIndAnalysis formula var
depIndAnalysis (And f1 f2) var = depIndAnalysis f1 var && depIndAnalysis f2 var
depIndAnalysis (Or f1 f2) var = depIndAnalysis f1 var && depIndAnalysis f2 var
depIndAnalysis (Not f1) var = depIndAnalysis f1 var
depIndAnalysis (Pt f1) var = depIndAnalysis f1 var
depIndAnalysis (Pf f1) var = depIndAnalysis f1 var
depIndAnalysis (P f1) var = depIndAnalysis f1 var
depIndAnalysis _ _ = Prelude.True

-- primitive licm, cannot peek more than one ast node down but is recursively applied at each level
licm :: MFormula -> MFormula
licm (Forall i _ _ f1) | depIndAnalysis f1 i = f1 -- todo: insert pf/pt in here?
licm (Forall i n m (And f1 f2)) | depIndAnalysis f1 i = And (Forall i n m f2) f1
licm (Forall i n m (And f1 f2)) | depIndAnalysis f2 i = And (Forall i n m f1) f2
licm (Forall i n m (Or f1 f2)) | depIndAnalysis f1 i = Or (Forall i n m f2) f1
licm (Forall i n m (Or f1 f2)) | depIndAnalysis f2 i = Or (Forall i n m f1) f2
licm (Exists i _ _ f1) | depIndAnalysis f1 i = f1
licm (Exists i n m (And f1 f2)) | depIndAnalysis f1 i = And (Exists i n m f2) f1
licm (Exists i n m (And f1 f2)) | depIndAnalysis f2 i = And (Exists i n m f1) f2
licm (Exists i n m (Or f1 f2)) | depIndAnalysis f1 i = Or (Exists i n m f2) f1
licm (Exists i n m (Or f1 f2)) | depIndAnalysis f2 i = Or (Exists i n m f1) f2
licm f1 | f1 == f1' = f1'
        | otherwise = licm f1' -- should reach a fixpoint
  where 
    f1' = thread f1 licm 

-- meval :: FreshVar -> TFormula -> AbsVar -> AbsVar -> MFormula
-- meval 2 form n m@(Add m' 1) = reduce $ seval fresh form n m

-- reduce  

{-
meval :: FreshVar -> TFormula -> AbsVar -> AbsVar -> MFormula
-- for the Stream case, we assume that n < m
meval fresh (Stream label) n m@(Add _ 1) = IndexedStream label n
meval fresh (LAnd f1 f2) n m@(Add _ 1) = And (meval fresh f1 n m ) (meval fresh f2 n m)
meval fresh (LOr f1 f2) n m@(Add _ 1) = Or (meval fresh f1 n m) (meval fresh f2 n m)
meval fresh (LNot f1) n m@(Add _ 1) = Not (meval fresh f1 n m)
-- where the *magic* happens
meval fresh (Globally f1) n m@(Add m' 1) = And (Forall i n m' (meval (fresh+1) f1 i m)) (meval fresh f1 m m) where i = gen fresh
meval fresh (Finally f1) n m@(Add m' 1) = Or (Exists i n m' (meval (fresh+1) f1 i m)) (meval fresh f1 m m) where i = gen fresh
-- todo: below, until case
meval fresh (Until f1 f2) n m@(Add m' 1) = Exists i n m' (And (Forall j n (Minus i 1) (f1')) f2')
  where 
    i = gen fresh
    j = gen (fresh+1)
    f1' = meval (fresh+2) f1 j m
    f2' = meval (fresh+2) f2 i m
meval fresh (Next f1) n m@(Add m' 1) = Pf (meval fresh f1 (Add n 1) m)
meval _ (Constant n) _ _ = Const n
meval _ _ _ _ = error "you should only be passing (m+1) to this func"
-}

-- nb: a compiler correctness proof would show that seval and meval are logically equivalent
