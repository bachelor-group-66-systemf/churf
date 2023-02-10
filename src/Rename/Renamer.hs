{-# LANGUAGE OverloadedRecordDot, LambdaCase, TypeFamilies, PatternSynonyms #-}

module Rename.Renamer where

import Abs

import qualified Grammar.Abs as A
import           Grammar.ErrM         (Err)
import           Control.Monad.Except (throwError)
import           Grammar.Print        (printTree)
import           Control.Monad.State
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

------------------ DATA TYPES ------------------

type Rn a = StateT Env Err a

data Env = Env { uniques :: Map String Unique
               , nextUnique :: Unique
               , sig :: Set String
               }

newtype Unique = Unique Int
    deriving Enum

data Name = Nu Unique | Ni String

initEnv :: Env
initEnv = Env
        { uniques    = mempty
        , nextUnique = Unique 0
        , sig        = mempty
        }

findBind :: String -> Rn Name
findBind x = lookupUnique x >>= \case
    Just u  -> pure $ Nu u
    Nothing -> gets (S.member x . sig) >>= \case
      False -> throwError ("Unbound variable " ++ printTree x)
      True  -> pure $ Ni x

newUnique :: String -> Rn Unique
newUnique x = do
  u <- gets nextUnique
  modify $ \env -> env { nextUnique = succ u
                       , uniques = M.insert x u env.uniques }
  return u

lookupUnique :: String -> Rn (Maybe Unique)
lookupUnique x = gets (M.lookup x . uniques)

renameDef :: Def -> Rn Def
renameDef = \case
  DExp x t _ xs e -> do
    newSig x
    xs' <- mapM newUnique xs
    e'  <- renameExp e
    let e'' = foldr ($) e' . zipWith R.EAbs xs' $ fromTree t
    pure . R.DBind $ R.Bind x t e''

renameExp :: A.Exp -> Rn ExpRE
renameExp e =
    case e of
   A.EInt i -> pure (EIntR i)
   A.EId (A.Ident str) -> flip EIdR str <$> findBind str
   A.EAdd e1 e2 -> EAppR <$> renameExp e1 <*> renameExp e2
   A.EApp e1 e2 -> EAppR <$> renameExp e1 <*> renameExp e2
   A.EAbs (A.Ident x) e -> do
       x' <- newUnique x
       e' <- renameExp e
       pure $ EAbsR x' x e'

data R
type ExpRE = Exp R

type instance XInt R = ()
type instance XId  R = Name
type instance XAdd R = ()
type instance XApp R = ()
type instance XAbs R = Unique
type instance XExp R = ()

pattern EIntR :: Integer -> ExpRE
pattern EIntR i = EInt () i

pattern EIdR :: Name -> String -> ExpRE
pattern EIdR n s = EId n s

pattern EAppR :: ExpRE -> ExpRE -> ExpRE
pattern EAppR e1 e2 = EApp () e1 e2

pattern EAbsR :: Unique -> String -> ExpRE -> ExpRE
pattern EAbsR u n e = EAbs u n e
