{-# LANGUAGE LambdaCase #-}

module TypeChecker.RemoveTEVar where

import Control.Applicative (Applicative (liftA2), liftA3)
import Control.Monad.Except (MonadError (throwError))
import Data.Coerce (coerce)
import Data.Tuple.Extra (secondM)
import Grammar.Abs
import Grammar.ErrM (Err)
import TypeChecker.TypeCheckerIr qualified as T

class RemoveTEVar a b where
    rmTEVar :: a -> Err b

instance RemoveTEVar (T.Program' Type) (T.Program' T.Type) where
    rmTEVar (T.Program defs) = T.Program <$> rmTEVar defs

instance RemoveTEVar (T.Def' Type) (T.Def' T.Type) where
    rmTEVar = \case
        T.DBind bind -> T.DBind <$> rmTEVar bind
        T.DData dat -> T.DData <$> rmTEVar dat

instance RemoveTEVar (T.Bind' Type) (T.Bind' T.Type) where
    rmTEVar (T.Bind id vars rhs) = liftA3 T.Bind (rmTEVar id) (rmTEVar vars) (rmTEVar rhs)

instance RemoveTEVar (T.Exp' Type) (T.Exp' T.Type) where
    rmTEVar exp = case exp of
        T.EVar name -> pure $ T.EVar name
        T.EInj name -> pure $ T.EInj name
        T.ELit lit -> pure $ T.ELit lit
        T.ELet bind e -> liftA2 T.ELet (rmTEVar bind) (rmTEVar e)
        T.EApp e1 e2 -> liftA2 T.EApp (rmTEVar e1) (rmTEVar e2)
        T.EAdd e1 e2 -> liftA2 T.EAdd (rmTEVar e1) (rmTEVar e2)
        T.EAbs name e -> T.EAbs name <$> rmTEVar e
        T.ECase e branches -> liftA2 T.ECase (rmTEVar e) (rmTEVar branches)

instance RemoveTEVar (T.Branch' Type) (T.Branch' T.Type) where
    rmTEVar (T.Branch (patt, t_patt) e) = liftA2 T.Branch (liftA2 (,) (rmTEVar patt) (rmTEVar t_patt)) (rmTEVar e)

instance RemoveTEVar (T.Pattern' Type) (T.Pattern' T.Type) where
    rmTEVar = \case
        T.PVar (name, t) -> T.PVar . (name,) <$> rmTEVar t
        T.PLit (lit, t) -> T.PLit . (lit,) <$> rmTEVar t
        T.PCatch -> pure T.PCatch
        T.PEnum name -> pure $ T.PEnum name
        T.PInj name ps -> T.PInj name <$> rmTEVar ps

instance RemoveTEVar (T.Data' Type) (T.Data' T.Type) where
    rmTEVar (T.Data typ injs) = liftA2 T.Data (rmTEVar typ) (rmTEVar injs)

instance RemoveTEVar (T.Inj' Type) (T.Inj' T.Type) where
    rmTEVar (T.Inj name typ) = T.Inj name <$> rmTEVar typ

instance RemoveTEVar (T.Id' Type) (T.Id' T.Type) where
    rmTEVar = secondM rmTEVar

instance RemoveTEVar (T.ExpT' Type) (T.ExpT' T.Type) where
    rmTEVar (exp, typ) = liftA2 (,) (rmTEVar exp) (rmTEVar typ)

instance RemoveTEVar a b => RemoveTEVar [a] [b] where
    rmTEVar = mapM rmTEVar

instance RemoveTEVar Type T.Type where
    rmTEVar = \case
        TLit lit -> pure $ T.TLit (coerce lit)
        TVar tvar -> pure $ T.TVar (coerce tvar)
        TData name typs -> T.TData (coerce name) <$> rmTEVar typs
        TFun t1 t2 -> liftA2 T.TFun (rmTEVar t1) (rmTEVar t2)
        TAll tvar t -> T.TAll (coerce tvar) <$> rmTEVar t
        TEVar _ -> throwError "NewType TEVar!"
