{-# LANGUAGE LambdaCase #-}

module Monomorphizer.Monomorphizer (monomorphize) where

import           Grammar.Abs                   (Ident (..))
import           Monomorphizer.MonomorphizerIr
import qualified Monomorphizer.MonomorphizerIr as M
import qualified TypeChecker.TypeCheckerIr     as T

monomorphize :: T.Program -> Program
monomorphize (T.Program ds) = Program $ monoDefs ds

monoDefs :: [T.Def] -> [Def]
monoDefs = map monoDef

monoDef :: T.Def -> Def
monoDef (T.DBind bind) = DBind $ monoBind bind
monoDef (T.DData d)    = DData d

monoBind :: T.Bind -> Bind
monoBind (T.Bind name args (e, t)) = Bind name args (monoExpr e, monoType t)

monoExpr :: T.Exp -> M.Exp
monoExpr = \case
    T.EId (Ident i) -> EId (Ident i)
    T.ELit lit -> ELit $ monoLit lit
    T.ELet bind expt -> ELet (monoBind bind) (monoexpt expt)
    T.EApp expt1 expt2 -> EApp (monoexpt expt1) (monoexpt expt2)
    T.EAdd expt1 expt2 -> EAdd (monoexpt expt1) (monoexpt expt2)
    T.EAbs i expt -> error "BUG"
    T.ECase expt injs -> ECase (monoexpt expt) (monoInjs injs)

monoType :: T.Type -> Type
monoType (T.TAll _ t)          = monoType t
monoType (T.TVar (T.MkTVar i)) = error "NOT POLYMORPHIC TYPES"
monoType (T.TLit i)            = TLit i
monoType (T.TFun t1 t2)        = TFun (monoType t1) (monoType t2)

monoexpt :: T.ExpT -> M.ExpT
monoexpt (e, t) = (monoExpr e, monoType t)

monoId :: T.Id -> Id
monoId = id

monoLit :: T.Lit -> Lit
monoLit (T.LInt i)  = LInt i
monoLit (T.LChar c) = LChar c

monoInjs = map monoInj

monoInj (T.Inj (init, t) expt) = Injection (monoInit init, monoType t) (monoexpt expt)

monoInit :: T.Init -> Init
monoInit = id
