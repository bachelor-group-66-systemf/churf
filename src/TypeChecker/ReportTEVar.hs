{-# LANGUAGE LambdaCase #-}

module TypeChecker.ReportTEVar where

import           Auxiliary                 (onM)
import           Control.Applicative       (Applicative (liftA2), liftA3)
import           Control.Monad.Except      (MonadError (throwError))
import           Data.Coerce               (coerce)
import           Data.Tuple.Extra          (secondM)
import qualified Grammar.Abs               as G
import           Grammar.ErrM              (Err)
import           Grammar.Print             (printTree)
import           TypeChecker.TypeCheckerIr hiding (Type (..))


data Type
    = TLit Ident
    | TVar TVar
    | TData Ident [Type]
    | TFun Type Type
    | TAll TVar Type
    deriving (Eq, Ord, Show, Read)

class ReportTEVar a b where
    reportTEVar :: a -> Err b

instance ReportTEVar (Program' G.Type) (Program' Type) where
    reportTEVar (Program defs) = Program <$> reportTEVar defs

instance ReportTEVar (Def' G.Type) (Def' Type) where
    reportTEVar = \case
        DBind bind -> DBind <$> reportTEVar bind
        DData dat  -> DData <$> reportTEVar dat

instance ReportTEVar (Bind' G.Type) (Bind' Type) where
    reportTEVar (Bind id vars rhs) = liftA3 Bind (reportTEVar id) (reportTEVar vars) (reportTEVar rhs)

instance ReportTEVar (Exp' G.Type) (Exp' Type) where
    reportTEVar exp = case exp of
        EVar name        -> pure $ EVar name
        EInj name        -> pure $ EInj name
        ELit lit         -> pure $ ELit lit
        ELet bind e      -> liftA2 ELet (reportTEVar bind) (reportTEVar e)
        EApp e1 e2       -> onM EApp reportTEVar e1 e2
        EAdd e1 e2       -> onM EAdd reportTEVar e1 e2
        EAbs name e      -> EAbs name <$> reportTEVar e
        ECase e branches -> liftA2 ECase (reportTEVar e) (reportTEVar branches)

instance ReportTEVar (Branch' G.Type) (Branch' Type) where
    reportTEVar (Branch (patt, t_patt) e) = liftA2 Branch (liftA2 (,) (reportTEVar patt) (reportTEVar t_patt)) (reportTEVar e)

instance ReportTEVar (Pattern' G.Type, G.Type) (Pattern' Type, Type) where
    reportTEVar (p, t) = liftA2 (,) (reportTEVar p) (reportTEVar t)

instance ReportTEVar (Pattern' G.Type) (Pattern' Type) where
    reportTEVar = \case
        PVar name    -> pure $ PVar name
        PLit lit     -> pure $ PLit lit
        PCatch       -> pure PCatch
        PEnum name   -> pure $ PEnum name
        PInj name ps -> PInj name <$> reportTEVar ps

instance ReportTEVar (Data' G.Type) (Data' Type) where
    reportTEVar (Data typ injs) = liftA2 Data (reportTEVar typ) (reportTEVar injs)

instance ReportTEVar (Inj' G.Type) (Inj' Type) where
    reportTEVar (Inj name typ) = Inj name <$> reportTEVar typ

instance ReportTEVar (Id' G.Type) (Id' Type) where
    reportTEVar = secondM reportTEVar

instance ReportTEVar (ExpT' G.Type) (ExpT' Type) where
    reportTEVar (exp, typ) = liftA2 (,) (reportTEVar exp) (reportTEVar typ)

instance ReportTEVar a b => ReportTEVar [a] [b] where
    reportTEVar = mapM reportTEVar

instance ReportTEVar G.Type Type where
    reportTEVar = \case
        G.TLit lit            -> pure $ TLit (coerce lit)
        G.TVar (G.MkTVar i)   -> pure $ TVar (MkTVar $ coerce i)
        G.TData name typs     -> TData (coerce name) <$> reportTEVar typs
        G.TFun t1 t2          -> liftA2 TFun (reportTEVar t1) (reportTEVar t2)
        G.TAll (G.MkTVar i) t -> TAll (MkTVar $ coerce i) <$> reportTEVar t
        G.TEVar tevar         -> throwError ("Found TEVar: " ++ printTree tevar)
