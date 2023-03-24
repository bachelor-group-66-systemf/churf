{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Renamer.Renamer (rename) where

import Auxiliary (mapAccumM)
import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (
    MonadState,
    StateT,
    evalStateT,
    gets,
    modify,
 )
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (dupe)
import Grammar.Abs

-- | Rename all variables and local binds
rename :: Program -> Either String Program
rename (Program defs) = Program <$> renameDefs defs

renameDefs :: [Def] -> Either String [Def]
renameDefs defs = runIdentity $ runExceptT $ evalStateT (runRn $ mapM renameDef defs) initCxt
  where
    initNames = Map.fromList [dupe (coerce name) | DBind (Bind name _ _) <- defs]

    renameDef :: Def -> Rn Def
    renameDef = \case
        DSig (Sig name typ) -> DSig . Sig name <$> renameTVars typ
        DBind bind -> DBind . snd <$> renameBind initNames bind
        DData (Data (TData cname types) constrs) -> do
            tvars_ <- tvars
            tvars' <- mapM nextNameTVar tvars_
            let tvars_lt = zip tvars_ tvars'
                typ' = map (substituteTVar tvars_lt) types
                constrs' = map (renameConstr tvars_lt) constrs
            pure . DData $ Data (TData cname typ') constrs'
          where
            tvars = concat <$> mapM (collectTVars []) types
            collectTVars :: [TVar] -> Type -> Rn [TVar]
            collectTVars tvars = \case
                TAll tvar t -> collectTVars (tvar : tvars) t
                TData _ _ -> return tvars
                -- Should be monad error
                TVar v -> return [v]
                _ -> throwError ("Bad data type definition: " ++ show types)
        DData (Data types _) -> throwError ("Bad data type definition: " ++ show types)

    renameConstr :: [(TVar, TVar)] -> Constructor -> Constructor
    renameConstr new_types (Constructor name typ) =
        Constructor name $ substituteTVar new_types typ

renameBind :: Names -> Bind -> Rn (Names, Bind)
renameBind old_names (Bind name vars rhs) = do
    (new_names, vars') <- newNames old_names (coerce vars)
    (newer_names, rhs') <- renameExp new_names rhs
    pure (newer_names, Bind name (coerce vars') rhs')

substituteTVar :: [(TVar, TVar)] -> Type -> Type
substituteTVar new_names typ = case typ of
    TLit _ -> typ
    TVar tvar
        | Just tvar' <- lookup tvar new_names ->
            TVar tvar'
        | otherwise ->
            typ
    TFun t1 t2 -> on TFun substitute' t1 t2
    TAll tvar t
        | Just tvar' <- lookup tvar new_names ->
            TAll tvar' $ substitute' t
        | otherwise ->
            TAll tvar $ substitute' t
    TData name typs -> TData name $ map substitute' typs
    _ -> error ("Impossible " ++ show typ)
  where
    substitute' = substituteTVar new_names

initCxt :: Cxt
initCxt = Cxt 0 0

data Cxt = Cxt
    { var_counter :: Int
    , tvar_counter :: Int
    }

-- | Rename monad. State holds the number of renamed names.
newtype Rn a = Rn {runRn :: StateT Cxt (ExceptT String Identity) a}
    deriving (Functor, Applicative, Monad, MonadState Cxt, MonadError String)

-- | Maps old to new name
type Names = Map LIdent LIdent

renameExp :: Names -> Exp -> Rn (Names, Exp)
renameExp old_names = \case
    EVar n -> pure (coerce old_names, EVar . fromMaybe n $ Map.lookup n old_names)
    ECons n -> pure (old_names, ECons n)
    ELit lit -> pure (old_names, ELit lit)
    EApp e1 e2 -> do
        (env1, e1') <- renameExp old_names e1
        (env2, e2') <- renameExp old_names e2
        pure (Map.union env1 env2, EApp e1' e2')
    EAdd e1 e2 -> do
        (env1, e1') <- renameExp old_names e1
        (env2, e2') <- renameExp old_names e2
        pure (Map.union env1 env2, EAdd e1' e2')

    -- TODO fix shadowing
    ELet bind e -> do
        (new_names, bind') <- renameBind old_names bind
        (new_names', e') <- renameExp new_names e
        pure (new_names', ELet bind' e')
    EAbs par e -> do
        (new_names, par') <- newName old_names (coerce par)
        (new_names', e') <- renameExp new_names e
        pure (new_names', EAbs (coerce par') e')
    EAnn e t -> do
        (new_names, e') <- renameExp old_names e
        t' <- renameTVars t
        pure (new_names, EAnn e' t')
    ECase e injs -> do
        (new_names, e') <- renameExp old_names e
        (new_names', injs') <- renameInjs new_names injs
        pure (new_names', ECase e' injs')

renameInjs :: Names -> [Inj] -> Rn (Names, [Inj])
renameInjs ns xs = do
    (new_names, xs') <- unzip <$> mapM (renameInj ns) xs
    if null new_names then return (mempty, xs') else return (head new_names, xs')

renameInj :: Names -> Inj -> Rn (Names, Inj)
renameInj ns (Inj init e) = do
    (new_names, init') <- renameInit ns init
    (new_names', e') <- renameExp new_names e
    return (new_names', Inj init' e')

renameInit :: Names -> Init -> Rn (Names, Init)
renameInit ns i = case i of
    InitConstructor cs vars -> do
        (ns_new, vars') <- newNames ns (coerce vars)
        return (ns_new, InitConstructor cs (coerce vars'))
    rest -> return (ns, rest)

renameTVars :: Type -> Rn Type
renameTVars typ = case typ of
    TAll tvar t -> do
        tvar' <- nextNameTVar tvar
        t' <- renameTVars $ substitute tvar tvar' t
        pure $ TAll tvar' t'
    TFun t1 t2 -> liftA2 TFun (renameTVars t1) (renameTVars t2)
    _ -> pure typ

substitute ::
    TVar -> -- α
    TVar -> -- α_n
    Type -> -- A
    Type -- [α_n/α]A
substitute tvar1 tvar2 typ = case typ of
    TLit _ -> typ
    TVar tvar'
        | tvar' == tvar1 -> TVar tvar2
        | otherwise -> typ
    TFun t1 t2 -> on TFun substitute' t1 t2
    TAll tvar t -> TAll tvar $ substitute' t
    TData name typs -> TData name $ map substitute' typs
    _ -> error "Impossible"
  where
    substitute' = substitute tvar1 tvar2

-- | Create a new name and add it to name environment.
newName :: Names -> LIdent -> Rn (Names, LIdent)
newName env old_name = do
    new_name <- makeName old_name
    pure (Map.insert old_name new_name env, new_name)

-- | Create multiple names and add them to the name environment
newNames :: Names -> [LIdent] -> Rn (Names, [LIdent])
newNames = mapAccumM newName

-- | Annotate name with number and increment the number @prefix ⇒ prefix_number@.
makeName :: LIdent -> Rn LIdent
makeName (LIdent prefix) = do
    i <- gets var_counter
    let name = LIdent $ prefix ++ "_" ++ show i
    modify $ \cxt -> cxt{var_counter = succ cxt.var_counter}
    pure name

nextNameTVar :: TVar -> Rn TVar
nextNameTVar (MkTVar (LIdent s)) = do
    i <- gets tvar_counter
    let tvar = MkTVar $ coerce $ s ++ "_" ++ show i
    modify $ \cxt -> cxt{tvar_counter = succ cxt.tvar_counter}
    pure tvar
