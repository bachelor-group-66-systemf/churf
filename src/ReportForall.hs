{-# LANGUAGE LambdaCase #-}

module ReportForall (reportForall) where

import           Auxiliary               (partitionDefs)
import           Control.Monad           (unless, void, when)
import           Control.Monad.Except    (MonadError (throwError))
import           Data.Either.Combinators (mapRight)
import           Data.Foldable           (foldlM)
import           Data.Function           (on)
import           Data.List               (delete)
import           Grammar.Abs
import           Grammar.ErrM            (Err)
import           TypeChecker.TypeChecker (TypeChecker (Bi, Hm))

reportForall :: TypeChecker -> Program -> Err ()
reportForall tc p = do
    when (tc == Hm) $ rpProgram rpaType p
    rpProgram rpuType p

rpuType :: Type -> Err ()
rpuType typ = do
    tvars <- go [] typ
    unless (null tvars) $ throwError "Unused forall"
  where
    go tvars = \case
        TAll tvar t
            | tvar `elem` tvars -> throwError "Unused forall"
            | otherwise         -> go (tvar : tvars) t
        TVar tvar -> pure (delete tvar tvars)
        TFun t1 t2 -> go tvars t1 >>= (`go` t2)
        TData _ typs -> foldlM go tvars typs
        _ -> pure tvars


rpaType :: Type -> Err ()
rpaType = rpForall . skipForall
  where
    skipForall = \case
        TAll _ t -> skipForall t
        t        -> t
    rpForall = \case
        TAll {}      -> throwError "Higher rank forall not allowed"
        TFun t1 t2   -> on (>>) rpForall t1 t2
        TData _ typs -> mapM_ rpForall typs
        _            -> pure ()

rpProgram :: (Type -> Err ()) -> Program -> Err ()
rpProgram rf (Program defs) = do
    mapM_ rpuBind bs
    mapM_ rpuData ds
    mapM_ rpuSig ss
  where
    (ds, ss, bs) = partitionDefs defs
    rpuSig (Sig _ typ) = rf typ
    rpuData (Data typ injs) = rf typ >> mapM rpuInj injs
    rpuInj (Inj _ typ) = rf typ
    rpuBind (Bind _ _ rhs) = rpuExp rhs
    rpuBranch (Branch _ e) = rpuExp e
    rpuExp = \case
        EAnn e t    -> rpuExp e >> rf t
        EApp e1 e2  -> on (>>) rpuExp e1 e2
        EAdd e1 e2  -> on (>>) rpuExp e1 e2
        ELet bind e -> rpuBind bind >> rpuExp e
        EAbs _ e    -> rpuExp e
        ECase e bs  -> rpuExp e >> mapM_ rpuBranch bs
        _           -> pure ()

reportAnyForall :: Program -> Err ()
reportAnyForall = undefined
