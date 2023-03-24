{-# LANGUAGE LambdaCase #-}

module Monomorphizer.Monomorphizer (monomorphize) where

import           Data.Coerce                   (coerce)
import           Grammar.Abs                   (Constructor (..), Ident (..),
                                                Indexed (..))
import qualified Grammar.Abs                   as GA
import qualified Monomorphizer.MonomorphizerIr as M
import qualified TypeChecker.TypeCheckerIr     as T

monomorphize :: T.Program -> M.Program
monomorphize (T.Program ds) = M.Program $ monoDefs ds

monoDefs :: [T.Def] -> [M.Def]
monoDefs = map monoDef

monoDef :: T.Def -> M.Def
monoDef (T.DBind bind) = M.DBind $ monoBind bind
monoDef (T.DData d)    = M.DData $ monoData d

monoBind :: T.Bind -> M.Bind
monoBind (T.Bind name args (e, t)) = M.Bind (monoId name) (map monoId args) (monoExpr e, monoType t)

monoData :: T.Data -> M.Constructor
monoData (T.Data (Indexed n _) cons) =  M.Constructor n (map (\(Constructor n t) -> (n, monoAbsType t)) cons)

monoExpr :: T.Exp -> M.Exp
monoExpr = \case
    T.EId (Ident i) -> M.EId (Ident i)
    T.ELit lit -> M.ELit $ monoLit lit
    T.ELet bind expt -> M.ELet (monoBind bind) (monoexpt expt)
    T.EApp expt1 expt2 -> M.EApp (monoexpt expt1) (monoexpt expt2)
    T.EAdd expt1 expt2 -> M.EAdd (monoexpt expt1) (monoexpt expt2)
    T.EAbs _i _expt -> error "BUG"
    T.ECase expt injs -> M.ECase (monoexpt expt) (monoInjs injs)

monoAbsType :: GA.Type -> M.Type
monoAbsType (GA.TLit u)      = M.TLit (coerce u)
monoAbsType (GA.TVar _v)     = error "NOT POLYMORHPIC TYPES"
monoAbsType (GA.TAll _v _t)  = error "NOT ALL TYPES"
monoAbsType (GA.TIndexed _i) = error "NOT INDEXED TYPES"
monoAbsType (GA.TEVar _v)    = error "I DONT KNOW WHAT THIS IS"
monoAbsType (GA.TFun t1 t2)  = M.TFun (monoAbsType t1) (monoAbsType t2)


monoType :: T.Type -> M.Type
monoType (T.TAll _ t)          = monoType t
monoType (T.TVar (T.MkTVar i)) = error "NOT POLYMORPHIC TYPES"
monoType (T.TLit i)            = M.TLit i
monoType (T.TFun t1 t2)        = M.TFun (monoType t1) (monoType t2)
monoType (T.TIndexed _)        = error "Not sure what this is"

monoexpt :: T.ExpT -> M.ExpT
monoexpt (e, t) = (monoExpr e, monoType t)

monoId :: T.Id -> M.Id
monoId (n,t) = (n, monoType t)

monoLit :: T.Lit -> M.Lit
monoLit (T.LInt i)  = M.LInt i
monoLit (T.LChar c) = M.LChar c

monoInjs :: [T.Inj] -> [M.Injection]
monoInjs = map monoInj

monoInj :: T.Inj -> M.Injection
monoInj (T.Inj (init, t) expt) = M.Injection (monoInit init, monoType t) (monoexpt expt)

monoInit :: T.Init -> M.Init
monoInit = id

