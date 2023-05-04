{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AnnForall (annotateForall) where

import           Auxiliary            (partitionDefs)
import           Control.Applicative  (Applicative (liftA2))
import           Control.Monad.Except (throwError)
import           Data.Function        (on)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Grammar.Abs
import           Grammar.ErrM         (Err)

annotateForall :: Program -> Err Program
annotateForall (Program defs) = do
    ds' <- mapM (fmap DData . annData) ds
    bs' <- mapM (fmap DBind . annBind) bs
    pure $ Program (ds' ++ ss' ++ bs')
  where
    ss' = map (DSig . annSig) ss
    (ds, ss, bs) = partitionDefs defs


annData :: Data -> Err Data
annData (Data typ injs) = do
  (typ', tvars) <- annTyp typ
  pure (Data typ' $ map (annInj tvars) injs)

  where
    annTyp typ = do
        (bounded, ts) <- boundedTVars mempty typ
        unbounded <- Set.fromList <$> mapM assertTVar ts
        let diff = unbounded Set.\\ bounded
            typ' = foldr TAll typ diff
        (typ', ) . fst <$> boundedTVars mempty typ'
      where
        boundedTVars tvars typ = case typ of
          TAll tvar t -> boundedTVars (Set.insert tvar tvars) t
          TData _ ts  -> pure (tvars, ts)
          _           -> throwError "Misformed data declaration"

        assertTVar typ = case typ of
            TVar tvar -> pure tvar
            _         -> throwError $ unwords [ "Misformed data declaration:"
                                              , "Non type variable argument"
                                              ]
    annInj tvars (Inj n t) =
        Inj n $ foldr TAll t (unboundedTVars t Set.\\ tvars)

annSig :: Sig -> Sig
annSig (Sig name typ) = Sig name $ annType typ

annBind :: Bind -> Err Bind
annBind (Bind name vars exp) = Bind name vars <$> annExp exp
  where
    annExp = \case
        EAnn e t    -> flip EAnn (annType t) <$> annExp e
        EApp e1 e2  -> liftA2 EApp (annExp e1) (annExp e2)
        EAdd e1 e2  -> liftA2 EAdd (annExp e1) (annExp e2)
        ELet bind e -> liftA2 ELet (annBind bind) (annExp e)
        EAbs x e    -> EAbs x <$> annExp e
        ECase e bs  -> liftA2 ECase (annExp e) (mapM annBranch bs)
        e           -> pure e
    annBranch (Branch p e) = Branch p <$> annExp e

annType :: Type -> Type
annType typ = go $ unboundedTVars typ
  where
    go us
        | null us   = typ
        | otherwise = foldr TAll typ us

unboundedTVars :: Type -> Set TVar
unboundedTVars = unboundedTVars' mempty

unboundedTVars' :: Set TVar -> Type -> Set TVar
unboundedTVars' bs typ = tvars.unbounded Set.\\ tvars.bounded
  where
    tvars = gatherTVars typ
    gatherTVars = \case
      TAll tvar t  -> TVars { bounded   = Set.singleton tvar
                            , unbounded = unboundedTVars' (Set.insert tvar bs) t
                            }
      TVar tvar    -> uTVars $ Set.singleton tvar
      TFun t1 t2   -> uTVars $ on Set.union (unboundedTVars' bs) t1  t2
      TData _ typs -> uTVars $ foldr (Set.union . unboundedTVars' bs) mempty typs
      _            -> TVars { bounded = mempty, unbounded = mempty }

data TVars = TVars
  { bounded   :: Set TVar
  , unbounded :: Set TVar
  } deriving (Eq, Show, Ord)

uTVars :: Set TVar -> TVars
uTVars us = TVars
    { bounded   = mempty
    , unbounded = us
    }

