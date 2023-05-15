{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | For now, converts polymorphic functions to concrete ones based on usage.
Assumes lambdas are lifted.

This step of compilation is as follows:

Split all function bindings into monomorphic and polymorphic binds. The
monomorphic bindings will be part of this compilation step.
Apply the following monomorphization function on all monomorphic binds, with
their type as an additional argument.

The function that transforms Binds operates on both monomorphic and
polymorphic functions, creates a context in which all possible polymorphic types
are mapped to concrete types, created using the additional argument.
Expressions are then recursively processed. The type of these expressions
are changed to using the mapped generic types. The expected type provided
in the recursion is changed depending on the different nodes.

When an external bind is encountered (with EId), it is checked whether it
exists in outputed binds or not. If it does, nothing further is evaluated.
If not, the bind transformer function is called on it with the
expected type in this context. The result of this computation (a monomorphic
bind) is added to the resulting set of binds.
-}
module Monomorphizer.Monomorphizer (monomorphize, morphExp, morphBind) where


import           Control.Monad.Reader          (MonadReader (ask, local),
                                                ReaderT, asks, runReaderT)
import           Control.Monad.State           (MonadState,
                                                StateT (runStateT), gets,
                                                modify)
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Grammar.Print                 (printTree)
import           Monomorphizer.DataTypeRemover (removeDataTypes)
import qualified Monomorphizer.MonomorphizerIr as O
import qualified Monomorphizer.MorbIr          as M
import           LambdaLifterIr                (Ident (..))
import qualified LambdaLifterIr                as L

import           Data.Maybe                    (fromJust, catMaybes)
import           Data.Tuple.Extra              (secondM)
import Control.Monad.Except (throwError, Except, runExcept, MonadError)
import Data.List (foldl')

{- | EnvM is the monad containing the read-only state as well as the
output state containing monomorphized functions and to-be monomorphized
data type declarations.
-}
newtype EnvM a = EnvM (StateT Output (ReaderT Env (Except String)) a)
    deriving (Functor, Applicative, Monad, MonadState Output, MonadReader Env, MonadError String)

type Output = Map.Map Ident Outputted

{- | Data structure describing outputted top-level information, that is
Binds, Polymorphic Data types (monomorphized in a later step) and
Marked bind, which means that it is in the process of monomorphization
and should not be monomorphized again.
-}
data Outputted = Marked | Complete M.Bind | Data M.Type L.Data deriving (Show)

-- | Static environment.
data Env = Env
    { input    :: Map.Map Ident L.Bind
    -- ^ All binds in the program.
    , dataDefs :: Map.Map Ident L.Data
    -- ^ All constructors mapped to their respective polymorphic data def
    -- which includes all other constructors.
    , polys    :: Map.Map Ident M.Type
    -- ^ Maps polymorphic identifiers with concrete types.
    , locals   :: Set.Set Ident
    -- ^ Local variables.
    }

-- | Determines if the identifier describes a local variable in the given context.
localExists :: Ident -> EnvM Bool
localExists ident = asks (Set.member ident . locals)

-- | Gets a polymorphic bind from an id.
getInputBind :: Ident -> EnvM (Maybe L.Bind)
getInputBind ident = asks (Map.lookup ident . input)

-- | Add monomorphic function derived from a polymorphic one, to env.
addOutputBind :: M.Bind -> EnvM ()
addOutputBind b@(M.Bind (ident, _) _ _) = modify (Map.insert ident (Complete b))
addOutputBind b@(M.BindC _ (ident, _) _ _) = modify (Map.insert ident (Complete b))

{- | Marks a global bind as being processed, meaning that when encountered again,
it should not be recursively processed.
-}
markBind :: Ident -> EnvM ()
markBind ident = modify (Map.insert ident Marked)

-- | Check if bind has been touched or not.
isBindMarked :: Ident -> EnvM Bool
isBindMarked ident = gets (Map.member ident)

-- | Checks if constructor is outputted.
isConsMarked :: Ident -> EnvM Bool
isConsMarked ident = gets (Map.member ident)

-- | Finds main bind.
getMain :: EnvM L.Bind
getMain = do
    env <- ask
    case Map.lookup (Ident "main") (input env) of
          Just mainBind -> return mainBind
          Nothing -> throwError "main not found in monomorphizer!"

{- | Makes a kv pair list of polymorphic to monomorphic mappings, throws runtime
error when encountering different structures between the two arguments. Debug:
First argument is the name of the bind.
-}
mapTypes :: Ident -> L.Type -> M.Type -> EnvM [(Ident, M.Type)]
mapTypes _ident (L.TLit _) (M.TLit _) = return []
mapTypes _ident (L.TVar (L.MkTVar i1)) tm = return [(i1, tm)]
mapTypes ident (L.TFun pt1 pt2) (M.TFun mt1 mt2) =
    (++) <$> mapTypes ident pt1 mt1 <*> mapTypes ident pt2 mt2
mapTypes ident (L.TData tIdent pTs) (M.TData mIdent mTs) =
    if tIdent /= mIdent
        then throwError "the data type names of monomorphic and polymorphic data types does not match"
        else foldl' (\xs (p, m) -> do x <- mapTypes ident p m; (++x) <$> xs) (return []) (zip pTs mTs)
-- This is a proper callstack error as a previous phase has a bug.
mapTypes ident t1 t2 = error $ "in bind: '" ++ printTree ident ++ "', " ++
  "structure of types not the same: '" ++ printTree t1 ++ "', '" ++ printTree t2 ++ "'"

-- | Gets the mapped monomorphic type of a polymorphic type in the current context.
getMonoFromPoly :: L.Type -> EnvM M.Type
getMonoFromPoly t = do
    env <- ask
    return $ getMono (polys env) t
  where
    getMono :: Map.Map Ident M.Type -> L.Type -> M.Type
    getMono polys t = case t of
        (L.TLit ident) -> M.TLit ident
        (L.TFun t1 t2) -> M.TFun (getMono polys t1) (getMono polys t2)
        (L.TVar (L.MkTVar ident)) -> case Map.lookup ident polys of
            Just concrete -> concrete
            Nothing       -> M.TLit (Ident "void")
        -- error $ "type not found! type: " ++ show ident ++ ", error in previous compilation steps"
        (L.TData ident args) -> M.TData ident (map (getMono polys) args)

-- | Converts a monomorphic type to the output tree if that type is monomorphic
getMonoFromMono :: L.Type -> Maybe M.Type
getMonoFromMono t = case t of
        L.TLit ident -> Just $ M.TLit ident
        L.TFun t1 t2 -> do
            t1' <- getMonoFromMono t1
            t2' <- getMonoFromMono t2
            return $ M.TFun t1' t2'
        L.TVar _ -> Nothing
        L.TData ident args -> do
          args' <- mapM getMonoFromMono args
          return $ M.TData ident args'

{- | If ident not already in env's output, morphed bind to output
(and all referenced binds within this bind).
Returns the annotated bind name.
-}
morphBind :: M.Type -> L.Bind -> EnvM Ident
morphBind expectedType b@(L.Bind (ident, btype) args (exp, expt)) = do
    -- The "new name" is used to find out if it is already marked or not.
    let name' = newFuncName expectedType b
    bindMarked <- isBindMarked name'
    mt <- mapTypes ident btype expectedType
    local
        ( \env ->
            env
                { locals = Set.fromList (map fst args)
                , polys = Map.fromList mt
                }
        )
        $ do
            -- Return with right name if already marked
            if bindMarked
                then return name'
                else do
                    -- Mark so that this bind will not be processed in recursive or cyclic
                    -- function calls
                    markBind name'
                    expt' <- getMonoFromPoly expt
                    exp' <- morphExp expt' exp
                    -- Get monomorphic type sof args
                    args' <- mapM morphArg args
                    addOutputBind $
                        M.Bind
                            (name', expectedType)
                            args'
                            (exp', expt')
                    return name'

morphBind expectedType b@(L.BindC cxt (ident, btype) args (exp, expt)) = do
    -- The "new name" is used to find out if it is already marked or not.
    let name' = newFuncName expectedType b
    bindMarked <- isBindMarked name'
    mt <- mapTypes ident btype expectedType
    local
        ( \env ->
            env
                { locals = Set.fromList (map fst args)
                , polys = Map.fromList mt
                }
        )
        $ do
            -- Return with right name if already marked
            if bindMarked
                then return name'
                else do
                    -- Mark so that this bind will not be processed in recursive or cyclic
                    -- function calls
                    markBind name'
                    -- Get monomorphic type sof args
                    args' <- mapM morphArg args
                    cxt' <- mapM (secondM getMonoFromPoly) cxt
                    expt' <- getMonoFromPoly expt
                    exp' <- local (\env -> foldr (addLocal . fst) env cxt)
                                  (morphExp expt' exp)
                    addOutputBind $
                        M.BindC cxt'
                            (name', expectedType)
                            args'
                            (exp', expt')
                    return name'


-- | Monomorphizes arguments of a bind.
morphArg :: (Ident, L.Type) -> EnvM (Ident, M.Type)
morphArg (ident, t) = do
    t' <- getMonoFromPoly t
    return (ident, t')

-- | Gets the data bind from the name of a constructor.
getInputData :: Ident -> EnvM (Maybe L.Data)
getInputData ident = do
    asks (Map.lookup ident . dataDefs)

{- | Monomorphize a constructor using it's global name. Constructors may
appear as expressions in the tree, or as patterns in case-expressions.
'newIdent' has a unique name while 'ident' has a general name.
-}
morphCons :: M.Type -> Ident -> Ident -> EnvM ()
morphCons expectedType ident newIdent = do
    maybeD <- getInputData ident
    case maybeD of
        -- closures can have unbound variables
        Nothing -> pure ()
        Just d -> do
            modify (Map.insert newIdent (Data expectedType d))

-- | Converts literals from input to output tree.
convertLit :: L.Lit -> M.Lit
convertLit (L.LInt v)  = M.LInt v
convertLit (L.LChar v) = M.LChar v
convertLit l           = error $ "Unexpected lit in monomorphizer: '" ++ printTree l ++ "'"


-- | Monomorphizes an expression, given an expected type.
morphExp :: M.Type -> L.Exp -> EnvM M.Exp
morphExp expectedType exp = case exp of
    L.ELit lit -> return $ M.ELit lit
    -- Constructor
    L.EInj ident -> do
      let ident' = newName (getDataType expectedType) ident
      morphCons expectedType ident ident'
      return $ M.EVar ident'
    L.EApp (e1, _t1) (e2, t2) -> do
        t2' <- getMonoFromPoly t2
        e2' <- morphExp t2' e2
        e1' <- morphExp (M.TFun t2' expectedType) e1
        return $ M.EApp (e1', M.TFun t2' expectedType) (e2', t2')
    L.EAdd (e1, t1) (e2, t2) -> do
        t1' <- getMonoFromPoly t1
        t2' <- getMonoFromPoly t2
        e1' <- morphExp t1' e1
        e2' <- morphExp t2' e2
        return $ M.EAdd (e1', expectedType) (e2', expectedType)
    L.ECase (exp, t) bs -> do
        t' <- getMonoFromPoly t
        exp' <- morphExp t' exp
        bs' <- mapM morphBranch bs
        return $ M.ECase (exp', t') (catMaybes bs')
    -- Ideally constructors should be EInj, though this code handles them
    -- as well.
    -- FIXME MAKE EVAR AND EINJ SEPARATE!!!
    L.EVar ident -> do
        isLocal <- localExists ident
        if isLocal
            then do
                return $ M.EVar ident
            else do
                bind <- getInputBind ident
                case bind of
                    Nothing -> throwError $ "unbound variable: '" ++ printTree ident ++ "'"
                    Just bind' -> do
                        -- New bind to process
                        newBindName <- morphBind expectedType bind'
                        return $ M.EVar newBindName
    L.EVarC as ident -> do
        isLocal <- localExists ident
        if isLocal
            then do
                return $ M.EVar ident
            else do
                bind <- fromJust <$> getInputBind ident
                as' <- mapM (secondM getMonoFromPoly) as
                -- New bind to process
                newBindName <- morphBind expectedType bind
                return $ M.EVarC as' newBindName
    -- Ideally constructors should be EInj, though this code handles them
    -- as well.


    L.ELet (identB, tB) (expB, tExpB) (exp, tExp) -> do
        tB' <- getMonoFromPoly tB
        tExpB' <- getMonoFromPoly tExpB
        tExp' <- getMonoFromPoly tExp
        expB' <- morphExp tExpB' expB
        exp' <- local (addLocal identB) (morphExp tExp' exp)
        return $ M.ELet (M.Bind (identB, tB') [] (expB', tExpB')) (exp', tExp')

-- | Monomorphizes case-of branches.
morphBranch :: L.Branch -> EnvM (Maybe M.Branch)
morphBranch (L.Branch (p, pt) (e, et)) = do
  pt' <- getMonoFromPoly pt
  et' <- getMonoFromPoly et
  env <- ask
  maybeMorphedPattern <- morphPattern p pt'
  case maybeMorphedPattern of
    Nothing -> return Nothing
    Just (p', newLocals) ->
      local (const env { locals = Set.union (locals env) newLocals }) $ do
        e' <- morphExp et' e
        return $ Just (M.Branch p' (e', et'))

morphPattern :: L.Pattern -> M.Type -> EnvM (Maybe (M.T M.Pattern, Set.Set Ident))
morphPattern p expectedType = case p of
  L.PVar ident     -> return $ Just ((M.PVar ident, expectedType), Set.singleton ident)
  L.PLit lit       -> return $ Just ((M.PLit (convertLit lit), expectedType), Set.empty)
  L.PCatch         -> return $ Just ((M.PCatch, expectedType), Set.empty)
  L.PEnum ident    -> do
    let newIdent = newName expectedType ident
    morphCons expectedType ident newIdent
    return $ Just ((M.PEnum newIdent, expectedType), Set.empty)
  L.PInj ident pts -> do let newIdent = newName expectedType ident
                         ts' <- mapM (getMonoFromPoly . snd) pts
                         morphCons (convertConsTypeToDataType expectedType (reverse ts')) ident newIdent
                         let pts' = zip (map fst pts) ts'
                         psSets <- mapM (uncurry morphPattern) pts'
                         let maybePsSets = sequence psSets
                         case maybePsSets of
                           Nothing      -> return Nothing
                           Just psSets' -> return $ Just
                             ((M.PInj newIdent (map fst psSets'), expectedType), Set.unions $ map snd psSets')

-- Exampel: List a  =>  a -> List a
convertConsTypeToDataType :: M.Type -> [M.Type] -> M.Type
convertConsTypeToDataType = foldl (flip M.TFun)


-- | Creates a new identifier for a function with an assigned type.
newFuncName :: M.Type -> L.Bind -> Ident
newFuncName t (L.Bind (ident@(Ident bindName), _) _ _) =
    if bindName == "main"
        then Ident bindName
        else newName t ident

newFuncName t (L.BindC _ (ident@(Ident bindName), _) _ _) =
    if bindName == "main"
        then Ident bindName
        else newName t ident

newName :: M.Type -> Ident -> Ident
newName t (Ident str) = Ident $ str ++ "$" ++ newName' t
  where
    newName' :: M.Type -> String
    newName' (M.TLit (Ident str)) = str
    newName' (M.TFun t1 t2) = newName' t1 ++ "_" ++ newName' t2
    newName' (M.TData (Ident str) ts) = str ++ foldl (\s t -> s ++ "." ++ newName' t) "" ts

-- | Monomorphization step.
monomorphize :: L.Program -> Either String O.Program
monomorphize (L.Program defs) = do
    op <- runEnvM Map.empty (createEnv defs) monomorphize'
    let prg = getDefsFromOutput op
    return . removeDataTypes $ M.Program prg
  where
    monomorphize' :: EnvM ()
    monomorphize' = do
        mainBind <- getMain
        case mainBind of
          (L.BindC {}) -> error "main should not be a BindC node"
          main@(L.Bind _ _ (_, mainType)) -> case getMonoFromMono mainType of
            Nothing -> throwError "main should be monomorphic"
            Just mainTypeMono -> do
              morphBind mainTypeMono main
              return ()

-- | Runs and gives the output binds.
runEnvM :: Output -> Env -> EnvM () -> Either String Output
runEnvM o env (EnvM stateM) = snd <$> runExcept (runReaderT (runStateT stateM o) env)

-- | Creates the environment based on the input binds.
createEnv :: [L.Def] -> Env
createEnv defs =
    Env
        { input = Map.fromList bindPairs
        , dataDefs = Map.fromList dataPairs
        , polys = Map.empty
        , locals = Set.empty
        }
  where
    bindPairs = (map (\b -> (getBindName b, b)) . getBindsFromDefs) defs
    dataPairs :: [(Ident, L.Data)]
    dataPairs = (foldl (\acc d@(L.Data _ cs) -> map ((,d) . getConsName) cs ++ acc) [] . getDataFromDefs) defs

-- | Gets a top-lefel function name.
getBindName :: L.Bind -> Ident
getBindName (L.Bind (ident, _) _ _)    = ident
getBindName (L.BindC _ (ident, _) _ _) = ident

-- Helper functions
-- Gets custom data declarations form defs.
getDataFromDefs :: [L.Def] -> [L.Data]
getDataFromDefs =
    foldl
        ( \bs -> \case
            L.DBind _ -> bs
            L.DData d -> d : bs
        )
        []

getConsName :: L.Inj -> Ident
getConsName (L.Inj ident _) = ident

getBindsFromDefs :: [L.Def] -> [L.Bind]
getBindsFromDefs =
    foldl
        ( \bs -> \case
            L.DBind b -> b : bs
            L.DData _ -> bs
        )
        []

getDefsFromOutput :: Output -> [M.Def]
getDefsFromOutput o =
    map M.DBind binds
        ++ (map (M.DData . snd) . Map.toList) (createNewData dataInput Map.empty)
  where
    (binds, dataInput) = splitBindsAndData o

-- | Splits the output into binds and data declaration components (used in createNewData)
splitBindsAndData :: Output -> ([M.Bind], [(Ident, M.Type, L.Data)])
splitBindsAndData output =
    foldl
        ( \(oBinds, oData) (ident, o) -> case o of
            Marked     -> error "internal bug in monomorphizer"
            Complete b -> (b : oBinds, oData)
            Data t d   -> (oBinds, (ident, t, d) : oData)
        )
        ([], [])
        (Map.toList output)

-- | Converts all found constructors to monomorphic data declarations.
createNewData :: [(Ident, M.Type, L.Data)] -> Map.Map Ident M.Data -> Map.Map Ident M.Data
createNewData [] o = o
createNewData ((consIdent, consType, polyData) : input) o =
    createNewData input $
        Map.insertWith
            (\_ (M.Data _ cs) -> M.Data newDataType (newCons : cs))
            newDataName
            (M.Data newDataType [newCons])
            o
  where
    L.Data (L.TData polyDataIdent _) _ = polyData
    newDataType = getDataType consType
    newDataName = newName newDataType polyDataIdent
    newCons = M.Inj consIdent consType

-- | Gets the Data Type of a constructor type (a -> Just a becomes Just a).
getDataType :: M.Type -> M.Type
getDataType (M.TFun _t1 t2)     = getDataType t2
getDataType tData@(M.TData _ _) = tData
getDataType _                   = error "Bug in previous phase of compilation"


addLocal :: Ident -> Env -> Env
addLocal x env = env { locals = Set.insert x env.locals }
