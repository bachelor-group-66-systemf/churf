{-# LANGUAGE LambdaCase #-}

module Renamer.Renamer where

import Auxiliary (mapAccumM)
import Control.Monad.State (
    MonadState,
    State,
    evalState,
    gets,
    modify,
 )
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (dupe)
import Debug.Trace (trace)
import Grammar.Abs

-- | Rename all variables and local binds
rename :: Program -> Program
rename (Program bs) = Program $ evalState (runRn $ mapM (renameSc initNames) bs) 0
  where
    -- initNames = Map.fromList $ map (\(Bind name _ _ _ _) -> dupe name) bs
    initNames = Map.fromList $ foldl' saveIfBind [] bs
    saveIfBind acc (DBind (Bind name _ _ _ _)) = dupe name : acc
    saveIfBind acc _ = acc
    renameSc :: Names -> Def -> Rn Def
    renameSc old_names (DBind (Bind name t _ parms rhs)) = do
        (new_names, parms') <- newNames old_names parms
        rhs' <- snd <$> renameExp new_names rhs
        pure . DBind $ Bind name t name parms' rhs'
    renameSc _ def = pure def

-- | Rename monad. State holds the number of renamed names.
newtype Rn a = Rn {runRn :: State Int a}
    deriving (Functor, Applicative, Monad, MonadState Int)

-- | Maps old to new name
type Names = Map Ident Ident

renameLocalBind :: Names -> Bind -> Rn (Names, Bind)
renameLocalBind old_names (Bind name t _ parms rhs) = do
    (new_names, name') <- newName old_names name
    (new_names', parms') <- newNames new_names parms
    (new_names'', rhs') <- renameExp new_names' rhs
    pure (new_names'', Bind name' t name' parms' rhs')

renameExp :: Names -> Exp -> Rn (Names, Exp)
renameExp old_names = \case
    EId n -> pure (old_names, EId . fromMaybe n $ Map.lookup n old_names)
    ELit (LInt i1) -> pure (old_names, ELit (LInt i1))
    EApp e1 e2 -> do
        (env1, e1') <- renameExp old_names e1
        (env2, e2') <- renameExp old_names e2
        pure (Map.union env1 env2, EApp e1' e2')
    EAdd e1 e2 -> do
        (env1, e1') <- renameExp old_names e1
        (env2, e2') <- renameExp old_names e2
        pure (Map.union env1 env2, EAdd e1' e2')
    ELet i e1 e2 -> do
        (new_names, e1') <- renameExp old_names e1
        (new_names', e2') <- renameExp new_names e2
        pure (new_names', ELet i e1' e2')
    EAbs par e -> do
        (new_names, par') <- newName old_names par
        (new_names', e') <- renameExp new_names e
        pure (new_names', EAbs par' e')
    EAnn e t -> do
        (new_names, e') <- renameExp old_names e
        pure (new_names, EAnn e' t)
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
    InitConstr cs vars -> do
        (ns_new, vars') <- newNames ns vars
        return (ns_new, InitConstr cs vars')
    rest -> return (ns, rest)

-- | Create a new name and add it to name environment.
newName :: Names -> Ident -> Rn (Names, Ident)
newName env old_name = do
    new_name <- makeName old_name
    pure (Map.insert old_name new_name env, new_name)

-- | Create multiple names and add them to the name environment
newNames :: Names -> [Ident] -> Rn (Names, [Ident])
newNames = mapAccumM newName

-- | Annotate name with number and increment the number @prefix ⇒ prefix_number@.
makeName :: Ident -> Rn Ident
makeName (Ident prefix) = gets (\i -> Ident $ prefix ++ "_" ++ show i) <* modify succ
