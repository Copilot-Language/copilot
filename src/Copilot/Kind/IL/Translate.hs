--------------------------------------------------------------------------------

module Copilot.Kind.IL.Translate ( translate ) where


import Copilot.Kind.IL.Spec
import Copilot.Kind.Misc.Casted

import qualified Copilot.Core as C

import Control.Monad.State.Lazy
import Text.Printf

--------------------------------------------------------------------------------

seqId :: C.Id -> SeqId
seqId = printf "s%d"

--------------------------------------------------------------------------------

-- | Translates a Copilot specification to an IL specification
-- | which is satisfiable iff no trigger can occur

translate :: C.Spec -> Spec
translate cspec = Spec modelInit modelRec invariants maxDepth seqDescrs
  where
    streams  = C.specStreams  cspec
    triggers = C.specTriggers cspec
      
    maxDepth = maximum . (map bufferSize) $ streams
    bufferSize (C.Stream { C.streamBuffer }) = length streamBuffer

    seqDescrs = map seqDescr streams

    modelRec  = map streamRec $ streams
    modelInit = concat . map streamInit $ streams
    invariants = map (unsafeGuard . C.triggerGuard) $ triggers 


unsafeGuard :: C.Expr Bool -> Constraint
unsafeGuard e = Op1 Bool Not (translateExpr Bool e)

seqDescr :: C.Stream -> SeqDescr
seqDescr (C.Stream { C.streamId, C.streamExprType })
  | isCastable streamExprType C.Bool = SeqDescr id Bool
  | otherwise = SeqDescr id Integer
  where id = seqId streamId

streamInit :: C.Stream -> [Constraint]
streamInit (C.Stream { C.streamId       = id
                     , C.streamBuffer   = b :: [val]
                     , C.streamExprType = ty}) = 

  zipWith initConstraint [0,1..] b
  
  where

    initConstraint :: Integer -> val -> Constraint
    initConstraint p v
      | isCastable ty C.Bool = mk Bool    EqB (extractB $ toDyn ty v)
      | otherwise            = mk Integer EqI (extractI $ toDyn ty v)
        
      where
      mk :: forall t . Type t -> Op2 t t Bool -> t -> Constraint
      mk t eqOp c =
        Op2 Bool eqOp (SVal t (seqId id) (Fixed p)) (Const t c)
    

                
streamRec :: C.Stream -> Constraint
streamRec (C.Stream { C.streamId       = id
                    , C.streamExpr     = e
                    , C.streamBuffer   = b
                    , C.streamExprType = ty})

  | isCastable ty C.Bool = mk Bool EqB
  | otherwise            = mk Integer EqI

  where

    depth = length b
                                   
    mk :: forall t . Type t -> Op2 t t Bool -> Constraint
    mk t eqOp =
       Op2 Bool eqOp
       (SVal t (seqId id) (_n_plus depth))
       (translateExpr t e)
             
--------------------------------------------------------------------------------

-- | Evaluated at point (_n_ + d)

translateExpr :: Type b -> C.Expr a -> Expr b
translateExpr Integer (C.Const t v) = Const Integer (extractI $ toDyn t v)
translateExpr Bool    (C.Const t v) = Const Bool    (extractB $ toDyn t v)

translateExpr t (C.Drop _ k id) =
  SVal t (seqId id) (_n_plus k)

translateExpr Bool (C.Op1 C.Not e) =
  Op1 Bool Not (translateExpr Bool e)

translateExpr Integer (C.Op2 op e1 e2) =
  Op2 Integer op' (translateExpr Integer e1) (translateExpr Integer e2)
  where
    op' = case op of
      C.Add _ -> Add
      C.Sub _ -> Sub
      C.Mul _ -> Mul
      _       -> error "Not handled operator"

translateExpr Bool (C.Op2 op e1 e2) = case op of
  C.Eq t -> eqExpr t
  C.Ne t -> Op1 Bool Not (eqExpr t)
  C.Le _ -> compExpr Le
  C.Lt _ -> compExpr Lt
  C.Ge _ -> compExpr Ge
  C.Gt _ -> compExpr Gt
  _      -> Op2 Bool op' (translateExpr Bool e1) (translateExpr Bool e2)
  
  where

    eqExpr :: forall t . C.Type t -> Expr Bool
    eqExpr t
      | isCastable t C.Bool =
        Op2 Bool EqB (translateExpr Bool e1) (translateExpr Bool e2)
      | otherwise =
        Op2 Bool EqI (translateExpr Integer e1) (translateExpr Integer e2)

    compExpr op =
      Op2 Bool op (translateExpr Integer e1) (translateExpr Integer e2)
    
    op' = case op of
      C.And -> And
      C.Or  -> Or
      _     -> error "Not handled operator"

translateExpr t (C.Op3 (C.Mux _) cond e1 e2) =
  Ite t (translateExpr Bool cond) (translateExpr t e1 ) (translateExpr t e2)

translateExpr _ _ = error "Not implemented yet"

--------------------------------------------------------------------------------
