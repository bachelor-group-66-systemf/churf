{-# LANGUAGE LambdaCase #-}

module Desugar.Desugar where

import Data.Function (on)
import Grammar.Abs

desugar :: Program -> Program
desugar (Program defs) = Program (map desugarDef defs)

desugarDef :: Def -> Def
desugarDef = \case
    DBind b -> DBind (desugarBind b)
    DSig sig -> DSig sig
    DData d -> DData d

desugarBind :: Bind -> Bind
desugarBind (Bind name args e) = Bind name args (desugarExp e)

desugarExp :: Exp -> Exp
desugarExp = \case
    EAppInf e2 e1 -> (EApp `on` desugarExp) e1 e2
    EApp e1 e2 -> (EApp `on` desugarExp) e1 e2
    EAdd e1 e2 -> (EAdd `on` desugarExp) e1 e2
    EAbs i e -> EAbs i (desugarExp e)
    ELet b e -> ELet (desugarBind b) (desugarExp e)
    ECase e br -> ECase (desugarExp e) (map desugarBranch br)
    EAnn e t -> EAnn (desugarExp e) t
    e -> e

desugarBranch :: Branch -> Branch
desugarBranch (Branch p e) = Branch p (desugarExp e)
