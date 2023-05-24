{-# LANGUAGE LambdaCase #-}

module TypeChecker.RemoveForall (removeForall) where

import           Auxiliary                 (onM)
import           Control.Applicative       (Applicative (liftA2))
import           Data.Function             (on)
import           Data.List                 (partition)
import           Data.Tuple.Extra          (second)
import           Grammar.ErrM              (Err)
import qualified TypeChecker.ReportTEVar   as R
import           TypeChecker.TypeCheckerIr

removeForall :: Program -> Program
removeForall (Program defs) = Program $ map (DData . rfData) ds
                                     ++ map (DBind . rfBind) bs
  where
    (ds, bs) = ([d | DData d <- defs ], [ b | DBind b <- defs ])
    rfData (Data typ injs) = Data (rfType typ) (map rfInj injs)
    rfInj (Inj name typ) = Inj name (rfType typ)
    rfBind (Bind name vars rhs) = Bind (rfId name) (map rfId vars) (rfExpT rhs)
    rfId = second rfType
    rfExpT (e, t) = (rfExp e, rfType t)
    rfExp = \case
        EApp e1 e2  -> on EApp rfExpT e1 e2
        EAdd e1 e2  -> on EAdd rfExpT e1 e2
        ELet bind e -> ELet (rfBind bind) (rfExpT e)
        EAbs name e -> EAbs name (rfExpT e)
        ECase e bs  -> ECase (rfExpT e) (map rfBranch bs)
        ELit lit    -> ELit lit
        EVar name   -> EVar name
        EInj name   -> EInj name
    rfBranch (Branch p e) = Branch (rfPatternT p) (rfExpT e)
    rfPatternT (p, t) = (rfPattern p, rfType t)
    rfPattern = \case
        PVar name    -> PVar name
        PLit lit     -> PLit lit
        PCatch       -> PCatch
        PEnum name   -> PEnum name
        PInj name ps -> PInj name (map rfPatternT ps)

rfType :: Type -> Type
rfType = \case
  TAll _ t      -> rfType t
  TFun t1 t2    -> on TFun rfType t1 t2
  TData name ts -> TData name (map rfType ts)
  TLit lit      -> TLit lit
  TVar tvar     -> TVar tvar

