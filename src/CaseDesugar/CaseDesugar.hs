{-# LANGUAGE LambdaCase #-}

module CaseDesugar.CaseDesugar (desuga) where

import CaseDesugar.CaseDesugarIr qualified as CIR
import TypeChecker.TypeCheckerIr qualified as TIR

desuga :: TIR.Program -> CIR.Program
desuga (TIR.Program x) = CIR.Program $ desugaDef <$> x

desugaDef :: TIR.Def -> CIR.Def
desugaDef (TIR.DBind bin@TIR.Bind{}) = CIR.DBind $ desugaBind bin
desugaDef (TIR.DData dat@TIR.Data{}) = CIR.DData $ desugaData dat

desugaData :: TIR.Data -> CIR.Data
desugaData (TIR.Data t injs) = CIR.Data (desugaType t) (desugaInj <$> injs)

desugaType :: TIR.Type -> CIR.Type
desugaType (TIR.TLit (TIR.Ident s)) = CIR.TLit (CIR.Ident s)
desugaType (TIR.TVar tv) = CIR.TVar (desugaTVar tv)
desugaType (TIR.TData (TIR.Ident s) ts) = CIR.TData (CIR.Ident s) (desugaType <$> ts)
desugaType (TIR.TFun t1 t2) = CIR.TFun (desugaType t1) (desugaType t2)
desugaType (TIR.TAll _ t1) = desugaType t1

desugaTVar :: TIR.TVar -> CIR.TVar
desugaTVar (TIR.MkTVar (TIR.Ident s)) = CIR.MkTVar (CIR.Ident s)

desugaInj :: TIR.Inj -> CIR.Inj
desugaInj (TIR.Inj (TIR.Ident s) t) = CIR.Inj (CIR.Ident s) (desugaType t)

desugaId :: TIR.Id -> CIR.Id
desugaId (TIR.Ident s, t) = (CIR.Ident s, desugaType t)

desugaBind :: TIR.Bind -> CIR.Bind
desugaBind (TIR.Bind id args exp) =
    CIR.Bind (desugaId id) (desugaId <$> args) (desugaExpT exp)

desugaExpT :: TIR.ExpT -> CIR.ExpT
desugaExpT (exp, t) = (desugaExp exp, desugaType t)

desugaExp :: TIR.Exp -> CIR.Exp
desugaExp (TIR.EVar (TIR.Ident s)) = CIR.EVar (CIR.Ident s)
desugaExp (TIR.EInj (TIR.Ident s)) = CIR.EInj (CIR.Ident s)
desugaExp (TIR.ELit lit) = CIR.ELit lit
desugaExp (TIR.ELet b e) = CIR.ELet (desugaBind b) (desugaExpT e)
desugaExp (TIR.EApp e1 e2) = CIR.EApp (desugaExpT e1) (desugaExpT e2)
desugaExp (TIR.EAdd e1 e2) = CIR.EAdd (desugaExpT e1) (desugaExpT e2)
desugaExp (TIR.EAbs (TIR.Ident s) e) = CIR.EAbs (CIR.Ident s) (desugaExpT e)
desugaExp (TIR.ECase e branches) = CIR.ECase (desugaExpT e) (desugaBranches branches)

desugaBranches :: [TIR.Branch] -> [CIR.Branch]
desugaBranches bs = do
    let injections = filter (\case (TIR.Branch (TIR.PInj{}, _) _) -> True; _ -> False) bs
    let patterns = filter (\case (TIR.Branch (TIR.PInj{}, _) _) -> True; _ -> False) bs
    undefined

desugaBranch :: TIR.Branch -> CIR.Branch
desugaBranch (TIR.Branch (TIR.PInj (TIR.Ident s) ps, pt) e) = do
    undefined
desugaBranch (TIR.Branch (p, pt) e) = do
    CIR.Branch
        ( case p of
            TIR.PVar id -> (CIR.PVar (desugaId id), desugaType pt)
            TIR.PLit (lit, t) -> (CIR.PLit (lit, desugaType t), desugaType pt)
            TIR.PCatch -> (CIR.PCatch, desugaType pt)
            TIR.PEnum (TIR.Ident s) -> (CIR.PEnum (CIR.Ident s), desugaType pt)
        )
        (desugaExpT e)

{-
case (Tupli 5 5) of
    Tupli 6 5 => 1
    Tupli _ x => 3
    x => 1
===
case (Tupli 5 5) of
    Tupli x y => case x of
        6 => case y of
            5 => 1
            x => 3
        _ => case y of
            x => 3
-}