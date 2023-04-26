{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Renamer.Renamer (rename) where

import           Auxiliary            (maybeToRightM, onM, partitionDefs)
import           Control.Applicative  (liftA2)
import           Control.Monad.Except (ExceptT, MonadError, runExceptT)
import           Control.Monad.State  (MonadState, State, evalState, gets,
                                       modify)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Tuple.Extra     (dupe)
import           Grammar.Abs
import           Grammar.ErrM         (Err)
import           Grammar.Print        (printTree)

-- | Rename all variables and local binds
rename :: Program -> Err Program
rename (Program defs) = rename' $ do
    ds' <- mapM (fmap DData . rnData) ds
    ss' <- mapM (fmap DSig . rnSig) ss
    bs' <- mapM (fmap DBind . rnTopBind) bs
    pure $ Program (ds' ++ ss' ++ bs')
  where
    (ds, ss, bs) = partitionDefs defs
    rename' = flip evalState initCxt
            . runExceptT
            . runRn
    initCxt = Cxt
        { counter = 0
        , names   = Map.fromList $ [ dupe n | Sig n _ <- ss ]
                                ++ [ dupe n | Bind n _ _ <- bs ]
        }
rnData :: Data -> Rn Data
rnData (Data typ injs) = liftA2 Data (rnType typ) (mapM rnInj injs)
  where
    rnInj (Inj name t) = Inj name <$> rnType t

rnSig :: Sig -> Rn Sig
rnSig (Sig name typ) = liftA2 Sig (getName name) (rnType typ)

rnType :: Type -> Rn Type
rnType = \case
    TVar (MkTVar name)   -> TVar . MkTVar <$> getName name
    TFun t1 t2           -> onM TFun (localNames . rnType) t1 t2
    TApp t1 t2           -> onM TFun (localNames . rnType) t1 t2
    TAll (MkTVar name) t -> liftA2 (TAll . MkTVar) (newName name) (rnType t)
    typ                  -> pure typ

rnTopBind :: Bind -> Rn Bind
rnTopBind = rnBind' False

rnLocalBind :: Bind -> Rn Bind
rnLocalBind = rnBind' True

rnBind' :: Bool -> Bind -> Rn Bind
rnBind' isLocal (Bind name vars rhs) = do
    name' <- if isLocal then newName name else getName name
    (vars', rhs') <- localNames $ liftA2 (,) (mapM newName vars) (rnExp rhs)
    pure (Bind name' vars' rhs')

rnExp :: Exp -> Rn Exp
rnExp = \case
    EVar x      -> EVar <$> getName x
    EInj x      -> pure (EInj x)
    ELit lit    -> pure (ELit lit)
    EApp e1 e2  -> onM EApp (localNames . rnExp) e1 e2
    EAdd e1 e2  -> onM EAdd (localNames . rnExp) e1 e2
    ELet bind e -> liftA2 ELet (rnLocalBind bind) (rnExp e)
    EAbs x e    -> liftA2 EAbs (newName x) (rnExp e)
    EAnn e t    -> liftA2 EAnn (rnExp e) (rnType t)
    ECase e bs  -> liftA2 ECase (rnExp e) (mapM (localNames . rnBranch) bs)

rnBranch :: Branch -> Rn Branch
rnBranch (Branch p e) = liftA2 Branch (rnPattern p) (rnExp e)

rnPattern :: Pattern -> Rn Pattern
rnPattern = \case
    PVar x       -> PVar <$> newName x
    PLit lit     -> pure (PLit lit)
    PCatch       -> pure PCatch
    PEnum name   -> pure (PEnum name)
    PInj name ps -> PInj name <$> mapM rnPattern ps

data Cxt = Cxt
    { counter :: Int
    , names   :: Map LIdent LIdent
    }

-- | Rename monad. State holds the number of renamed names.
newtype Rn a = Rn {runRn :: ExceptT String (State Cxt) a}
    deriving (Functor, Applicative, Monad, MonadState Cxt, MonadError String)

getName :: LIdent -> Rn LIdent
getName name = maybeToRightM err =<< gets (Map.lookup name . names)
  where err = "Can't find new name " ++ printTree name

newName :: LIdent -> Rn LIdent
newName name = do
    name' <- gets (mk name . counter)
    modify $ \cxt -> cxt { counter = succ cxt.counter
                         , names   = Map.insert name name' cxt.names
                         }
    pure name'
  where
    mk (LIdent name) i = LIdent ("$" ++ show i ++ name)

localNames :: MonadState Cxt m => m b -> m b
localNames m = do
    old_names <- gets names
    m <* modify ( \cxt' -> cxt' { names = old_names })
