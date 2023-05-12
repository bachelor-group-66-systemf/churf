module Expander where

import TypeChecker.TypeCheckerIr
import Control.Monad.State

type TExp = T' Exp' Type

type M = State Int

expand :: Program -> Program
expand (Program defs) = Program (map expandDef defs)

expandDef :: Def -> Def
expandDef (DBind bind) = DBind $ expandBind bind
expandDef d            = d

initialState = 0

expandBind :: Bind' Type -> Bind' Type
expandBind (Bind name args e)
    = Bind name args $ evalState (expandExp e) initialState

expandExp :: TExp -> M TExp
expandExp e = do
    case e of
        (EApp e1@(e_, _) e2@(_, _), t) -> do
            let sizeType = arrows t
            let sizeExp = apps e_
            let diff = sizeType - sizeExp
            e1' <- expandExp e1
            e2' <- expandExp e2
            apply diff (EApp e1' e2', t)
        (EVar _, t) -> do
            let sizeType = arrows t
            apply sizeType e
        e -> pure e

apply :: Int -> TExp -> M TExp
apply n (e, t)
  | n < 1 = pure (e, t)
  | otherwise = do
    fr <- fresh
    let (TFun t1 t2) = t
    e' <- apply (n - 1) (EApp (e,t) (EVar fr, t1), t2)
    pure (EAbs fr e', t)

-- Eerily similar functions
apps :: Exp -> Int
apps (EApp _ (e2, _)) = 1 + apps e2
apps _ = 0

arrows :: Type -> Int
arrows (TFun _ t2) = 1 + arrows t2
arrows _ = 0

fresh :: M Ident
fresh = do
   n <- get 
   put (n + 1)
   return (letters !! n)
  where
    letters :: [Ident]
    letters =
        map (Ident . ("eta$" ++)) $ [1 ..] >>= flip replicateM ['a' .. 'z']
