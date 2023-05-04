{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Desugar.Desugar (desugar) where

import Data.Function (on)
import Debug.Trace (traceShow)
import Grammar.Abs
import Grammar.Print

{-

The entire module should never have any catch all pattern matches as that
will disble warnings for when the grammar is expanded.

-}

desugar :: Program -> Program
desugar (Program defs) = Program (map desugarDef defs)

desugarVarName :: VarName -> LIdent
desugarVarName (VSymbol (Symbol i)) = LIdent i
desugarVarName (VIdent i) = i

desugarDef :: Def -> Def
desugarDef = \case
    DBind b -> DBind (desugarBind b)
    DSig sig -> DSig (desugarSig sig)
    DData d -> DData (desugarData d)

desugarBind :: Bind -> Bind
desugarBind (BindS name args e) = Bind (desugarVarName name) args (desugarExp e)
desugarBind (Bind name args e) = Bind name args (desugarExp e)

desugarSig :: Sig -> Sig
desugarSig (SigS ident typ) = Sig (desugarVarName ident) (desugarType typ)
desugarSig (Sig ident typ) = Sig ident (desugarType typ)

desugarData :: Data -> Data
desugarData (Data typ injs) = Data (desugarType typ) (map desugarInj injs)

desugarType :: Type -> Type
desugarType = \case
    TIdent (UIdent "Int") -> TLit "Int"
    TIdent (UIdent "Char") -> TLit "Char"
    TIdent ident -> TData ident []
    TApp t1 t2 ->
        let (name : tvars) = flatten t1 ++ [t2]
         in case name of
                TIdent ident -> TData ident (map desugarType tvars)
                _ -> error "desugarType in Desugar.hs is not implemented correctly"
    TLit l -> TLit l
    TVar v -> TVar v
    (TAll i t) -> TAll i (desugarType t)
    TFun t1 t2 -> TFun (desugarType t1) (desugarType t2)
    TEVar v -> TEVar v
    TData ident typ -> TData ident (map desugarType typ)
  where
    flatten :: Type -> [Type]
    flatten (TApp a b) = flatten a <> flatten b
    flatten a = [a]

desugarInj :: Inj -> Inj
desugarInj (Inj ident typ) = Inj ident (desugarType typ)

desugarExp :: Exp -> Exp
desugarExp = \case
    EApp e1 e2 -> EApp (desugarExp e1) (desugarExp e2)
    EAdd e1 e2 -> EAdd (desugarExp e1) (desugarExp e2)
    EAbs i e -> EAbs i (desugarExp e)
    ELet b e -> ELet (desugarBind b) (desugarExp e)
    ECase e br -> ECase (desugarExp e) (map desugarBranch br)
    EAnn e t -> EAnn (desugarExp e) t
    EVarS (VSymbol (Symbol symb)) -> EVar (LIdent symb)
    EVarS (VIdent ident) -> EVar ident
    EVar i -> EVar i
    ELit l -> ELit l
    EInj i -> EInj i

desugarBranch :: Branch -> Branch
desugarBranch (Branch p e) = Branch (desugarPattern p) (desugarExp e)

desugarPattern :: Pattern -> Pattern
desugarPattern = \case
    PVar ident -> PVar ident
    PLit lit -> PLit (desugarLit lit)
    PCatch -> PCatch
    PEnum ident -> PEnum ident
    PInj ident patterns -> PInj ident (map desugarPattern patterns)

desugarLit :: Lit -> Lit
desugarLit (LInt i) = LInt i
desugarLit (LChar c) = LChar c
