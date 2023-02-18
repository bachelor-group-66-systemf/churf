{-# LANGUAGE LambdaCase #-}

module TypeChecker.RemoveTEVar where

import           Control.Arrow             (Arrow (second))
import           Data.Coerce               (coerce)
import           Data.Function             (on)
import           Grammar.Abs
import qualified TypeChecker.TypeCheckerIr as T

class RemoveTEVar a b where
    rmTEVar :: a -> b

instance RemoveTEVar (T.Program' Type) (T.Program' T.Type) where
    rmTEVar (T.Program defs) = T.Program (rmTEVar defs)

instance RemoveTEVar (T.Def' Type) (T.Def' T.Type) where
    rmTEVar = \case
      T.DBind bind -> T.DBind (rmTEVar bind)
      T.DData dat  -> T.DData (rmTEVar dat)

instance RemoveTEVar (T.Bind' Type) (T.Bind' T.Type) where
    rmTEVar (T.Bind id vars rhs) = T.Bind (rmTEVar id) (rmTEVar vars) (rmTEVar rhs)

instance RemoveTEVar (T.Exp' Type) (T.Exp' T.Type) where
    rmTEVar exp = case exp of
     T.EVar name        -> T.EVar name
     T.EInj name        -> T.EInj name
     T.ELit lit         -> T.ELit lit
     T.ELet bind e      -> T.ELet (rmTEVar bind) (rmTEVar e)
     T.EApp e1 e2       -> on T.EApp rmTEVar e1 e2
     T.EAdd e1 e2       -> on T.EApp rmTEVar e1 e2
     T.EAbs name e      -> T.EAbs name (rmTEVar e)
     T.ECase e branches -> T.ECase (rmTEVar e) (rmTEVar branches)

instance RemoveTEVar (T.Branch' Type) (T.Branch' T.Type) where
    rmTEVar (T.Branch (patt, t_patt) e) = T.Branch (rmTEVar patt, rmTEVar t_patt) (rmTEVar e)

instance RemoveTEVar (T.Pattern' Type) (T.Pattern' T.Type) where
    rmTEVar = \case
        T.PVar (name, t) -> T.PVar (name, rmTEVar t)
        T.PLit (lit, t)  -> T.PLit (lit, rmTEVar t)
        T.PCatch         -> T.PCatch
        T.PEnum name     -> T.PEnum name
        T.PInj name ps   -> T.PInj name $ rmTEVar ps

instance RemoveTEVar (T.Data' Type) (T.Data' T.Type) where
    rmTEVar (T.Data typ injs) = T.Data (rmTEVar typ) (rmTEVar injs)

instance RemoveTEVar (T.Inj' Type) (T.Inj' T.Type) where
    rmTEVar (T.Inj name typ) = T.Inj name (rmTEVar typ)

instance RemoveTEVar (T.Id' Type) (T.Id' T.Type) where
    rmTEVar = second rmTEVar

instance RemoveTEVar (T.ExpT' Type) (T.ExpT' T.Type) where
    rmTEVar (exp, typ) = (rmTEVar exp, rmTEVar typ)

instance RemoveTEVar a b => RemoveTEVar [a] [b] where
    rmTEVar = map rmTEVar

instance RemoveTEVar Type T.Type where
    rmTEVar = \case
        TLit lit        -> T.TLit (coerce lit)
        TVar tvar       -> T.TVar tvar
        TData name typs -> T.TData (coerce name) $ rmTEVar typs
        TFun t1 t2      -> on T.TFun rmTEVar t1 t2
        TAll tvar t     -> T.TAll tvar $ rmTEVar t
        TEVar _         -> error "NewType TEVar!"
