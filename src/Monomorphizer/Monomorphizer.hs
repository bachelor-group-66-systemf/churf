{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Monomorphizer.Monomorphizer (monomorphize) where

import Data.Coerce (coerce)
import Grammar.Abs (Constructor (..), Ident (..))
import Unsafe.Coerce (unsafeCoerce)

import Grammar.Abs qualified as GA
import Monomorphizer.MonomorphizerIr qualified as M
import TypeChecker.TypeCheckerIr qualified as T

monomorphize :: T.Program -> M.Program
monomorphize (T.Program ds) = M.Program $ monoDefs ds

monoDefs :: [T.Def] -> [M.Def]
monoDefs = map monoDef

monoDef :: T.Def -> M.Def
monoDef (T.DBind bind) = M.DBind $ monoBind bind
monoDef (T.DData d) = M.DData $ unsafeCoerce d

monoBind :: T.Bind -> M.Bind
monoBind (T.Bind name args (e, t)) = M.Bind (monoId name) (map monoId args) (monoExpr e, monoType t)

monoExpr :: T.Exp -> M.Exp
monoExpr = \case
    T.EId (T.Ident i) -> M.EId (Ident i)
    T.ELit lit -> M.ELit $ monoLit lit
    T.ELet bind expt -> M.ELet (monoBind bind) (monoexpt expt)
    T.EApp expt1 expt2 -> M.EApp (monoexpt expt1) (monoexpt expt2)
    T.EAdd expt1 expt2 -> M.EAdd (monoexpt expt1) (monoexpt expt2)
    T.EAbs _i _expt -> error "BUG"
    T.ECase expt injs -> M.ECase (monoexpt expt) (monoInjs injs)

monoAbsType :: GA.Type -> M.Type
monoAbsType (GA.TLit u) = M.TLit (coerce u)
monoAbsType (GA.TVar _v) = M.TLit "Int"
monoAbsType (GA.TAll _v _t) = error "NOT ALL TYPES"
monoAbsType (GA.TEVar _v) = error "I DONT KNOW WHAT THIS IS"
monoAbsType (GA.TFun t1 t2) = M.TFun (monoAbsType t1) (monoAbsType t2)
monoAbsType (GA.TData _ _) = error "NOT INDEXED TYPES"

monoType :: T.Type -> M.Type
monoType (T.TAll _ t) = monoType t
monoType (T.TVar (T.MkTVar i)) = M.TLit "Int"
monoType (T.TLit (T.Ident i)) = M.TLit (Ident i)
monoType (T.TFun t1 t2) = M.TFun (monoType t1) (monoType t2)
monoType (T.TData _ _) = error "Not sure what this is"

monoexpt :: T.ExpT -> M.ExpT
monoexpt (e, t) = (monoExpr e, monoType t)

monoId :: T.Id -> M.Id
monoId (n, t) = (coerce n, monoType t)

monoLit :: T.Lit -> M.Lit
monoLit (T.LInt i) = M.LInt i
monoLit (T.LChar c) = M.LChar c

monoInjs :: [T.Branch] -> [M.Branch]
monoInjs = map monoInj

monoInj :: T.Branch -> M.Branch
monoInj (T.Branch (init, t) expt) = M.Branch (monoInit init, monoType t) (monoexpt expt)

monoInit :: T.Pattern -> M.Pattern
monoInit (T.PVar (id, t)) = M.PVar (coerce id, monoType t)
monoInit (T.PLit (lit, t)) = M.PLit (monoLit lit, monoType t)
monoInit (T.PInj id ps) = M.PInj (coerce id) (monoInit <$> ps)
monoInit T.PCatch = M.PCatch
