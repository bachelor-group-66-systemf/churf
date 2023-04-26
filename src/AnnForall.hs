{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AnnForall (annotateForall) where

import           Auxiliary            (onM, partitionDefs)
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
    annInj tvars (Inj n t) =
        Inj n $ foldr TAll t (unboundedTVars t Set.\\ tvars)

    annTyp typ = do
        bs <- bound typ
        us <- unbound typ
        pure (foldr TAll typ $ Set.difference bs us, bs)
      where
        bound = \case
          TAll tvar t -> Set.insert tvar <$> bound t
          TApp _ _    -> pure mempty
          TIdent _    -> pure mempty
          _           -> throwError "Misformed data declaration"

        unbound t = case skipForalls t of
            TApp t1 t2 -> onM Set.union unbound t1 t2
            TVar tvar  -> pure (Set.singleton tvar)
            TIdent _   -> pure mempty
            _          -> throwError "Misformed data declaration"

        skipForalls = \case
            TAll _ t -> skipForalls t
            t        -> t



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
      TApp t1 t2   -> uTVars $ on Set.union (unboundedTVars' bs) t1  t2
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

