{-# LANGUAGE LambdaCase #-}

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

import Monomorphizer.DataTypeRemover (removeDataTypes)
import Monomorphizer.MonomorphizerIr qualified as O
import Monomorphizer.MorbIr qualified as M
import TypeChecker.TypeCheckerIr (Ident (Ident))
import TypeChecker.TypeCheckerIr qualified as T

import Control.Monad.Reader (
    MonadReader (ask, local),
    Reader,
    asks,
    runReader,
 )
import Control.Monad.State (
    MonadState (get),
    StateT (runStateT),
    gets,
    modify,
 )
import Data.Coerce (coerce)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Grammar.Print (printTree)
import Debug.Trace (trace)

{- | EnvM is the monad containing the read-only state as well as the
output state containing monomorphized functions and to-be monomorphized
data type declarations.
-}
newtype EnvM a = EnvM (StateT Output (Reader Env) a)
    deriving (Functor, Applicative, Monad, MonadState Output, MonadReader Env)

type Output = Map.Map Ident Outputted

{- | Data structure describing outputted top-level information, that is
Binds, Polymorphic Data types (monomorphized in a later step) and
Marked bind, which means that it is in the process of monomorphization
and should not be monomorphized again.
-}
data Outputted = Marked | Complete M.Bind | Data M.Type T.Data deriving (Show)

-- | Static environment.
data Env = Env
    { input :: Map.Map Ident T.Bind
    -- ^ All binds in the program.
    , dataDefs :: Map.Map Ident T.Data
    -- ^ All constructors mapped to their respective polymorphic data def
    -- which includes all other constructors.
    , polys :: Map.Map Ident M.Type
    -- ^ Maps polymorphic identifiers with concrete types.
    , locals :: Set.Set Ident
    -- ^ Local variables.
    }

-- | Determines if the identifier describes a local variable in the given context.
localExists :: Ident -> EnvM Bool
localExists ident = asks (Set.member ident . locals)

-- | Gets a polymorphic bind from an id.
getInputBind :: Ident -> EnvM (Maybe T.Bind)
getInputBind ident = asks (Map.lookup ident . input)

-- | Add monomorphic function derived from a polymorphic one, to env.
addOutputBind :: M.Bind -> EnvM ()
addOutputBind b@(M.Bind (ident, _) _ _) = modify (Map.insert ident (Complete b))

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
getMain :: EnvM T.Bind
getMain = asks (\env -> case Map.lookup (T.Ident "main") (input env) of
                          Just mainBind -> mainBind
                          Nothing -> error "main not found in monomorphizer!"
                        )

{- | Makes a kv pair list of polymorphic to monomorphic mappings, throws runtime
error when encountering different structures between the two arguments. Debug:
First argument is the name of the bind.
-}
mapTypes :: Ident -> T.Type -> M.Type -> [(Ident, M.Type)]
mapTypes _ident (T.TLit _) (M.TLit _) = []
mapTypes _ident (T.TVar (T.MkTVar i1)) tm = [(i1, tm)]
mapTypes ident (T.TFun pt1 pt2) (M.TFun mt1 mt2) =
    mapTypes ident pt1 mt1
        ++ mapTypes ident pt2 mt2
mapTypes ident (T.TData tIdent pTs) (M.TData mIdent mTs) =
    if tIdent /= mIdent
        then error "the data type names of monomorphic and polymorphic data types does not match"
        else foldl (\xs (p, m) -> mapTypes ident p m ++ xs) [] (zip pTs mTs)
mapTypes ident t1 t2 = error $ "in bind: '" ++ printTree ident ++ "', " ++
  "structure of types not the same: '" ++ printTree t1 ++ "', '" ++ printTree t2 ++ "'"

-- | Gets the mapped monomorphic type of a polymorphic type in the current context.
getMonoFromPoly :: T.Type -> EnvM M.Type
getMonoFromPoly t = do
    env <- ask
    return $ getMono (polys env) t
  where
    getMono :: Map.Map Ident M.Type -> T.Type -> M.Type
    getMono polys t = case t of
        (T.TLit ident) -> M.TLit (coerce ident)
        (T.TFun t1 t2) -> M.TFun (getMono polys t1) (getMono polys t2)
        (T.TVar (T.MkTVar ident)) -> case Map.lookup ident polys of
            Just concrete -> concrete
            Nothing -> M.TLit (Ident "void")
        -- error $ "type not found! type: " ++ show ident ++ ", error in previous compilation steps"
        (T.TData ident args) -> M.TData ident (map (getMono polys) args)

{- | If ident not already in env's output, morphed bind to output
(and all referenced binds within this bind).
Returns the annotated bind name.
-}
morphBind :: M.Type -> T.Bind -> EnvM Ident
morphBind expectedType b@(T.Bind (ident, btype) args (exp, expt)) = do
    -- The "new name" is used to find out if it is already marked or not.
    let name' = newFuncName expectedType b
    bindMarked <- isBindMarked (coerce name')
    local
        ( \env ->
            env
                { locals = Set.fromList (map fst args)
                , polys = Map.fromList (mapTypes ident btype expectedType)
                }
        )
        $ do
            -- Return with right name if already marked
            if bindMarked
                then return name'
                else do
                    -- Mark so that this bind will not be processed in recursive or cyclic
                    -- function calls
                    markBind (coerce name')
                    expt' <- getMonoFromPoly expt
                    exp' <- morphExp expt' exp
                    -- Get monomorphic type sof args
                    args' <- mapM morphArg args
                    addOutputBind $
                        M.Bind
                            (coerce name', expectedType)
                            args'
                            (exp', expt')
                    return name'

-- | Monomorphizes arguments of a bind.
morphArg :: (Ident, T.Type) -> EnvM (Ident, M.Type)
morphArg (ident, t) = do
    t' <- getMonoFromPoly t
    return (ident, t')

-- | Gets the data bind from the name of a constructor.
getInputData :: Ident -> EnvM (Maybe T.Data)
getInputData ident = do
    env <- ask
    return $ Map.lookup ident (dataDefs env)

{- | Monomorphize a constructor using it's global name. Constructors may
appear as expressions in the tree, or as patterns in case-expressions.
'newIdent' has a unique name while 'ident' has a general name.
-}
morphCons :: M.Type -> Ident -> Ident -> EnvM ()
morphCons expectedType ident newIdent = do
    --trace ("Tjofras:" ++ show (newName expectedType ident)) $ return ()
    maybeD <- getInputData ident
    case maybeD of
        Nothing -> error $ "identifier '" ++ show ident ++ "' not found"
        Just d -> do
            modify (\output -> Map.insert newIdent (Data expectedType d) output)

-- | Converts literals from input to output tree.
convertLit :: T.Lit -> M.Lit
convertLit (T.LInt v) = M.LInt v
convertLit (T.LChar v) = M.LChar v

-- | Monomorphizes an expression, given an expected type.
morphExp :: M.Type -> T.Exp -> EnvM M.Exp
morphExp expectedType exp = case exp of
    T.ELit lit -> return $ M.ELit (convertLit lit)
    -- Constructor
    T.EInj ident -> do
      let ident' = newName (getDataType expectedType) ident
      morphCons expectedType ident ident'
      return $ M.EVar ident'
    T.EApp (e1, _t1) (e2, t2) -> do
        t2' <- getMonoFromPoly t2
        e2' <- morphExp t2' e2
        e1' <- morphExp (M.TFun t2' expectedType) e1
        return $ M.EApp (e1', M.TFun t2' expectedType) (e2', t2')
    T.EAdd (e1, t1) (e2, t2) -> do
        t1' <- getMonoFromPoly t1
        t2' <- getMonoFromPoly t2
        e1' <- morphExp t1' e1
        e2' <- morphExp t2' e2
        return $ M.EAdd (e1', expectedType) (e2', expectedType)
    T.EAbs ident (exp, t) -> local (\env -> env{locals = Set.insert ident (locals env)}) $ do
        t' <- getMonoFromPoly t
        morphExp t' exp
    T.ECase (exp, t) bs -> do
        t' <- getMonoFromPoly t
        exp' <- morphExp t' exp
        bs' <- mapM morphBranch bs
        return $ M.ECase (exp', t') (catMaybes bs')
    -- Ideally constructors should be EInj, though this code handles them
    -- as well.
    T.EVar ident -> do
        isLocal <- localExists ident
        if isLocal
            then do
                return $ M.EVar (coerce ident)
            else do
                bind <- getInputBind ident
                case bind of
                    Nothing -> error $ "unbound variable: '" ++ printTree ident ++ "'"
                    Just bind' -> do
                        -- New bind to process
                        newBindName <- morphBind expectedType bind'
                        return $ M.EVar (coerce newBindName)
    T.ELet (T.Bind (identB, tB) args (expB, tExpB)) (exp, tExp) ->
      if length args > 0 then error "only constants in lets allowed"
      else do
        tB' <- getMonoFromPoly tB
        tExpB' <- getMonoFromPoly tExpB
        tExp' <- getMonoFromPoly tExp
        expB' <- morphExp tExpB' expB
        exp' <- morphExp tExp' exp
        return $ M.ELet (M.Bind (identB, tB') [] (expB', tExpB')) (exp', tExp')

-- | Monomorphizes case-of branches.
morphBranch :: T.Branch -> EnvM (Maybe M.Branch)
morphBranch (T.Branch (p, pt) (e, et)) = do
  pt' <- getMonoFromPoly pt
  et' <- getMonoFromPoly et
  env <- ask
  maybeMorphedPattern <- morphPattern p pt'
  case maybeMorphedPattern of
    Nothing -> return Nothing
    Just (p', newLocals) -> 
      local (const env { locals = Set.union (locals env) newLocals }) $ do
        e' <- morphExp et' e
        return $ Just (M.Branch (p', pt') (e', et'))

morphPattern :: T.Pattern -> M.Type -> EnvM (Maybe (M.Pattern, Set.Set Ident))
morphPattern p expectedType = case p of
  T.PVar ident     -> return $ Just (M.PVar (ident, expectedType), Set.singleton ident)
  T.PLit lit       -> return $ Just (M.PLit (convertLit lit, expectedType), Set.empty)
  T.PCatch         -> return $ Just (M.PCatch, Set.empty)
  T.PEnum ident    -> return $ Just (M.PEnum (newName expectedType ident), Set.empty)
  T.PInj ident pts -> do let newIdent = newName expectedType ident
                         outEnv <- get
                         trace ("WOW: " ++ show (newName expectedType ident)) $ return ()
                         trace ("WOW2: " ++ show (outEnv)) $ return ()
                         isMarked <- isConsMarked newIdent
                         if isMarked
                            then do
                              trace ("WOW3") $ return ()
                              ts' <- mapM (getMonoFromPoly . snd) pts
                              let pts' = zip (map fst pts) ts'
                              psSets <- mapM (uncurry morphPattern) pts'
                              let maybePsSets = sequence psSets
                              case maybePsSets of
                                Nothing      -> return Nothing
                                Just psSets' -> return $ Just 
                                  (M.PInj newIdent (map fst psSets'), Set.unions $ map snd psSets')
                            else return Nothing

-- | Creates a new identifier for a function with an assigned type.
newFuncName :: M.Type -> T.Bind -> Ident
newFuncName t (T.Bind (ident@(Ident bindName), _) _ _) =
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
monomorphize :: T.Program -> O.Program
monomorphize (T.Program defs) =
    removeDataTypes $
        M.Program
            ( getDefsFromOutput
                (runEnvM Map.empty (createEnv defs) monomorphize')
            )
  where
    monomorphize' :: EnvM ()
    monomorphize' = do
        main <- getMain
        morphBind (M.TLit $ Ident "Int") main
        return ()

-- | Runs and gives the output binds.
runEnvM :: Output -> Env -> EnvM () -> Output
runEnvM o env (EnvM stateM) = snd $ runReader (runStateT stateM o) env

-- | Creates the environment based on the input binds.
createEnv :: [T.Def] -> Env
createEnv defs =
    Env
        { input = Map.fromList bindPairs
        , dataDefs = Map.fromList dataPairs
        , polys = Map.empty
        , locals = Set.empty
        }
  where
    bindPairs = (map (\b -> (getBindName b, b)) . getBindsFromDefs) defs
    dataPairs :: [(Ident, T.Data)]
    dataPairs = (foldl (\acc d@(T.Data _ cs) -> map ((,d) . getConsName) cs ++ acc) [] . getDataFromDefs) defs

-- | Gets a top-lefel function name.
getBindName :: T.Bind -> Ident
getBindName (T.Bind (ident, _) _ _) = ident

-- Helper functions
-- Gets custom data declarations form defs.
getDataFromDefs :: [T.Def] -> [T.Data]
getDataFromDefs =
    foldl
        ( \bs -> \case
            T.DBind _ -> bs
            T.DData d -> d : bs
        )
        []

getConsName :: T.Inj -> Ident
getConsName (T.Inj ident _) = ident

getBindsFromDefs :: [T.Def] -> [T.Bind]
getBindsFromDefs =
    foldl
        ( \bs -> \case
            T.DBind b -> b : bs
            T.DData _ -> bs
        )
        []

getDefsFromOutput :: Output -> [M.Def]
getDefsFromOutput o =
    map M.DBind binds
        ++ (map (M.DData . snd) . Map.toList) (createNewData dataInput Map.empty)
  where
    (binds, dataInput) = splitBindsAndData o

-- | Splits the output into binds and data declaration components (used in createNewData)
splitBindsAndData :: Output -> ([M.Bind], [(Ident, M.Type, T.Data)])
splitBindsAndData output =
    foldl
        ( \(oBinds, oData) (ident, o) -> case o of
            Marked -> error "internal bug in monomorphizer"
            Complete b -> (b : oBinds, oData)
            Data t d -> (oBinds, (ident, t, d) : oData)
        )
        ([], [])
        (Map.toList output)

-- | Converts all found constructors to monomorphic data declarations.
createNewData :: [(Ident, M.Type, T.Data)] -> Map.Map Ident M.Data -> Map.Map Ident M.Data
createNewData [] o = o
createNewData ((consIdent, consType, polyData) : input) o =
    createNewData input $
        Map.insertWith
            (\_ (M.Data _ cs) -> M.Data newDataType (newCons : cs))
            newDataName
            (M.Data newDataType [newCons])
            o
  where
    T.Data (T.TData polyDataIdent _) _ = polyData
    newDataType = getDataType consType
    newDataName = newName newDataType polyDataIdent
    newCons = M.Inj consIdent consType

-- | Gets the Data Type of a constructor type (a -> Just a becomes Just a).
getDataType :: M.Type -> M.Type
getDataType (M.TFun _t1 t2) = getDataType t2
getDataType tData@(M.TData _ _) = tData
getDataType _ = error "???"

