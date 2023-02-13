{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Renamer.Renamer (rename) where

import           Control.Applicative  (Applicative (liftA2))
import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.RWS    (MonadState, gets, modify)
import           Control.Monad.State  (StateT, evalStateT)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Grammar.Abs
import           Grammar.ErrM         (Err)
import           Grammar.Print        (printTree)
import qualified Renamer.RenamerIr            as R


data Cxt = Cxt
  { uniques    :: [(Ident, R.Unique)]
  , nextUnique :: R.Unique
  , sig        :: Set Ident
  }

initCxt :: Cxt
initCxt = Cxt
  { uniques    = []
  , nextUnique = R.Unique 0
  , sig        = mempty
  }

newtype Rn a = Rn { runRn :: StateT Cxt Err a }
  deriving (Functor, Applicative, Monad, MonadState Cxt, MonadError String)

rename :: Program -> Err R.Program
rename p = evalStateT (runRn $ renameProgram p) initCxt

renameProgram :: Program -> Rn R.Program
renameProgram (Program ds (Main e)) = do
  ds' <- mapM renameDef ds
  e'  <- renameExp e
  pure $ R.Program ds' (R.Main e')

renameDef :: Def -> Rn R.Def
renameDef = \case
  DExp x t _ xs e -> do
    newSig x
    xs' <- mapM newUnique xs
    e'  <- renameExp e
    let e'' = foldr ($) e' . zipWith R.EAbs xs' $ fromTree t
    pure . R.DBind $ R.Bind x t e''

renameExp :: Exp -> Rn R.Exp
renameExp = \case
  EId x      -> R.EId <$> findBind x
  EInt i     -> pure $ R.EInt i
  EApp e e1  -> liftA2 R.EApp (renameExp e) $ renameExp e1
  EAdd e e1  -> liftA2 R.EAdd (renameExp e) $ renameExp e1
  EAbs x t e -> do
    x' <- newUnique x
    e' <- renameExp e
    pure $ R.EAbs x' t e'

findBind :: Ident -> Rn R.Name
findBind x = lookupUnique x >>= \case
    Just u  -> pure $ R.Nu u
    Nothing -> gets (Set.member x . sig) >>= \case
      False -> throwError ("Unbound variable " ++ printTree x)
      True  -> pure $ R.Ni x

newUnique :: Ident -> Rn R.Unique
newUnique x = do
  u <- gets nextUnique
  modify $ \env -> env { nextUnique = succ u
                       , uniques = (x, u) : env.uniques
                       }
  pure u

newSig :: Ident -> Rn ()
newSig x = modify $ \cxt -> cxt { sig = Set.insert x cxt.sig}

lookupUnique :: Ident -> Rn (Maybe R.Unique)
lookupUnique x = lookup x <$> gets uniques

fromTree :: Type -> [Type]
fromTree = fromTree' []

fromTree' :: [Type] -> Type -> [Type]
fromTree' acc = \case
  TFun t t1 -> acc ++ [t] ++ fromTree t1
  other     -> other : acc
