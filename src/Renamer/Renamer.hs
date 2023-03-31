{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Renamer.Renamer (rename) where

import Auxiliary (mapAccumM)
import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (
    ExceptT,
    MonadError (throwError),
    runExceptT,
 )
import Control.Monad.State (
    MonadState,
    State,
    evalState,
    gets,
    mapAndUnzipM,
    modify,
 )
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (dupe, second)
import Grammar.Abs
import Grammar.ErrM (Err)
import Grammar.Print (printTree)

-- | Rename all variables and local binds
rename :: Program -> Err Program
rename (Program defs) = Program <$> renameDefs defs

initCxt :: Cxt
initCxt = Cxt 0 0

data Cxt = Cxt
    { var_counter :: Int
    , tvar_counter :: Int
    }

-- | Rename monad. State holds the number of renamed names.
newtype Rn a = Rn {runRn :: ExceptT String (State Cxt) a}
    deriving (Functor, Applicative, Monad, MonadState Cxt, MonadError String)

-- | Maps old to new name
type Names = Map String String

toStr :: VarName -> String
toStr (VarNameLIdent (LIdent i)) = i
toStr (VarNameSymbol (Symbol i)) = i

renameDefs :: [Def] -> Err [Def]
renameDefs defs = evalState (runExceptT (runRn $ mapM renameDef defs)) initCxt
  where
    initNames = Map.fromList [dupe (toStr n) | DBind (Bind n _ _) <- defs]

    renameDef :: Def -> Rn Def
    renameDef = \case
        DSig (Sig name typ) -> DSig . Sig name <$> renameTVars typ
        DBind (Bind name vars rhs) -> do
            (new_names, vars') <- newNamesL initNames vars
            rhs' <- snd <$> renameExp new_names rhs
            pure . DBind $ Bind name vars' rhs'
        DData (Data typ injs) -> do
            tvars <- collectTVars [] typ
            tvars' <- mapM nextNameTVar tvars
            let tvars_lt = zip tvars tvars'
                typ' = substituteTVar tvars_lt typ
                injs' = map (renameInj tvars_lt) injs
            pure . DData $ Data typ' injs'
          where
            collectTVars tvars = \case
                TAll tvar t -> collectTVars (tvar : tvars) t
                TData _ _ -> pure tvars
                _ -> throwError ("Bad data type definition: " ++ printTree typ)

    renameInj :: [(TVar, TVar)] -> Inj -> Inj
    renameInj new_types (Inj name typ) =
        Inj name $ substituteTVar new_types typ

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

renameExp :: Names -> Exp -> Rn (Names, Exp)
renameExp old_names = \case
    EVar n -> pure (old_names, EVar . VarNameLIdent . LIdent . fromMaybe (toStr n) $ Map.lookup (toStr n) old_names)
    EInj (UIdent n) -> pure (old_names, EInj . UIdent . fromMaybe n $ Map.lookup n old_names)
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
    ELet (Bind name vars rhs) e -> do
        (new_names, name') <- newNameL old_names (LIdent $ toStr name)
        (new_names', vars') <- newNamesL new_names vars
        (new_names'', rhs') <- renameExp new_names' rhs
        (new_names''', e') <- renameExp new_names'' e
        pure (new_names''', ELet (Bind (VarNameLIdent name') vars' rhs') e')
    EAbs par e -> do
        (new_names, par') <- newNameL old_names par
        (new_names', e') <- renameExp new_names e
        pure (new_names', EAbs par' e')
    EAnn e t -> do
        (new_names, e') <- renameExp old_names e
        t' <- renameTVars t
        pure (new_names, EAnn e' t')
    ECase e injs -> do
        (new_names, e') <- renameExp old_names e
        (new_names', injs') <- renameBranches new_names injs
        pure (new_names', ECase e' injs')

renameBranches :: Names -> [Branch] -> Rn (Names, [Branch])
renameBranches ns xs = do
    (new_names, xs') <- mapAndUnzipM (renameBranch ns) xs
    if null new_names then return (mempty, xs') else return (head new_names, xs')

renameBranch :: Names -> Branch -> Rn (Names, Branch)
renameBranch ns (Branch patt e) = do
    (new_names, patt') <- renamePattern ns patt
    (new_names', e') <- renameExp new_names e
    return (new_names', Branch patt' e')

renamePattern :: Names -> Pattern -> Rn (Names, Pattern)
renamePattern ns p = case p of
    PInj cs ps -> do
        (ns_new, ps') <- mapAccumM renamePattern ns ps
        return (ns_new, PInj cs ps')
    PVar name -> second PVar <$> newNameL ns name
    _ -> return (ns, p)

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
    TVar tvar
        | tvar == tvar1 -> TVar tvar2
        | otherwise -> typ
    TFun t1 t2 -> on TFun substitute' t1 t2
    TAll tvar t
        | tvar == tvar1 -> TAll tvar2 $ substitute' t
        | otherwise -> TAll tvar $ substitute' t
    TData name typs -> TData name $ map substitute' typs
    _ -> error "Impossible"
  where
    substitute' = substitute tvar1 tvar2

-- | Create multiple names and add them to the name environment
newNamesL :: Names -> [LIdent] -> Rn (Names, [LIdent])
newNamesL = mapAccumM newNameL

-- | Create a new name and add it to name environment.
newNameL :: Names -> LIdent -> Rn (Names, LIdent)
newNameL env (LIdent old_name) = do
    new_name <- makeName old_name
    pure (Map.insert old_name new_name env, LIdent new_name)

-- | Create multiple names and add them to the name environment
newNamesU :: Names -> [UIdent] -> Rn (Names, [UIdent])
newNamesU = mapAccumM newNameU

-- | Create a new name and add it to name environment.
newNameU :: Names -> UIdent -> Rn (Names, UIdent)
newNameU env (UIdent old_name) = do
    new_name <- makeName old_name
    pure (Map.insert old_name new_name env, UIdent new_name)

-- | Annotate name with number and increment the number @prefix ⇒ prefix_number@.
makeName :: String -> Rn String
makeName prefix = do
    i <- gets var_counter
    let name = prefix ++ "_" ++ show i
    modify $ \cxt -> cxt{var_counter = succ cxt.var_counter}
    pure name

nextNameTVar :: TVar -> Rn TVar
nextNameTVar (MkTVar (LIdent s)) = do
    i <- gets tvar_counter
    let tvar = MkTVar . LIdent $ s ++ "_" ++ show i
    modify $ \cxt -> cxt{tvar_counter = succ cxt.tvar_counter}
    pure tvar
