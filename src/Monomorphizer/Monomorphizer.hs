-- | For now, converts polymorphic functions to concrete ones based on usage.
-- Assumes lambdas are lifted.
--
-- This step of compilation is as follows:
--
-- Split all function bindings into monomorphic and polymorphic binds. The
-- monomorphic bindings will be part of this compilation step.
-- Apply the following monomorphization function on all monomorphic binds, with
-- their type as an additional argument.
--
-- The function that transforms Binds operates on both monomorphic and
-- polymorphic functions, creates a context in which all possible polymorphic types
-- are mapped to concrete types, created using the additional argument.
-- Expressions are then recursively processed. The type of these expressions
-- are changed to using the mapped generic types. The expected type provided
-- in the recursion is changed depending on the different nodes.
--
-- When an external bind is encountered (with EId), it is checked whether it
-- exists in outputed binds or not. If it does, nothing further is evaluated.
-- If not, the bind transformer function is called on it with the
-- expected type in this context. The result of this computation (a monomorphic
-- bind) is added to the resulting set of binds.

{-# LANGUAGE LambdaCase #-}
module Monomorphizer.Monomorphizer (monomorphize, morphExp, morphBind) where

import           Monomorphizer.DataTypeRemover (removeDataTypes)
import qualified Monomorphizer.MonomorphizerIr as O
import qualified Monomorphizer.MorbIr          as M
import qualified TypeChecker.TypeCheckerIr     as T
import           TypeChecker.TypeCheckerIr     (Ident (Ident))

import           Control.Monad.Reader          (MonadReader (ask, local),
                                                Reader, asks, runReader)
import           Control.Monad.State           (MonadState (get),
                                                StateT (runStateT), gets,
                                                modify)
import           Data.Coerce                   (coerce)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)
import qualified Data.Set                      as Set
import           Debug.Trace
import           Grammar.Print                 (printTree)

-- | State Monad wrapper for "Env".
newtype EnvM a = EnvM (StateT Output (Reader Env) a)
  deriving (Functor, Applicative, Monad, MonadState Output, MonadReader Env)

type Output = Map.Map Ident Outputted
-- When a bind is being processed, it is Incomplete in the state, also
-- called marked.
data Outputted = Incomplete | Complete M.Bind | Data M.Type T.Data

-- Static environment
data Env = Env {
  -- | All binds in the program.
  input    :: Map.Map Ident T.Bind,
  -- | All constructors and their respective data def.
  dataDefs ::  Map.Map Ident T.Data,
  -- | Maps polymorphic identifiers with concrete types.
  polys    :: Map.Map Ident M.Type,
  -- | Local variables.
  locals   :: Set.Set Ident
}

localExists :: Ident -> EnvM Bool
localExists ident = asks (Set.member ident . locals)

-- | Gets a polymorphic bind from an id.
getInputBind :: Ident -> EnvM (Maybe T.Bind)
getInputBind ident = asks (Map.lookup ident . input)

-- | Add monomorphic function derived from a polymorphic one, to env.
addOutputBind :: M.Bind -> EnvM ()
addOutputBind b@(M.Bind (ident, _) _ _) = modify (Map.insert ident (Complete b))

-- | Marks a global bind as being processed, meaning that when encountered again,
-- it should not be recursively processed.
markBind :: Ident -> EnvM ()
markBind ident = modify (Map.insert ident Incomplete)

-- | Check if bind has been touched or not.
isBindMarked :: Ident -> EnvM Bool
isBindMarked ident = gets (Map.member ident)

-- | Finds main bind
getMain :: EnvM T.Bind
getMain = asks (\env -> fromJust $ Map.lookup (T.Ident "main") (input env))

-- NOTE: could make this function more optimized
-- | Makes a kv pair list of polymorphic to monomorphic mappings, throws runtime
-- error when encountering different structures between the two arguments.
mapTypes :: T.Type -> M.Type -> [(Ident, M.Type)]
mapTypes (T.TLit _)              (M.TLit _) = []
mapTypes (T.TVar (T.MkTVar i1))  tm         = [(i1, tm)]
mapTypes (T.TFun pt1 pt2)        (M.TFun mt1 mt2)  = mapTypes pt1 mt1 ++
                                                     mapTypes pt2 mt2
mapTypes (T.TData tIdent pTs)    (M.TData mIdent mTs) = if tIdent /= mIdent
  then error "nuh uh"
  else foldl (\xs (p, m) -> mapTypes p m ++ xs) [] (zip pTs mTs)
mapTypes t1 t2 = error $ "structure of types not the same: '" ++ printTree t1 ++ "', '" ++ printTree t2 ++ "'"

-- | Gets the mapped monomorphic type of a polymorphic type in the current context.
getMonoFromPoly :: T.Type -> EnvM M.Type
getMonoFromPoly t = do env <- ask
                       return $ getMono (polys env) t
 where
  getMono :: Map.Map Ident M.Type -> T.Type -> M.Type
  getMono polys t = case t of
    (T.TLit ident)            -> M.TLit (coerce ident)
    (T.TFun t1 t2)            -> M.TFun (getMono polys t1) (getMono polys t2)
    (T.TVar (T.MkTVar ident)) -> case Map.lookup ident polys of
                         Just concrete -> concrete
                         Nothing       -> M.TLit (Ident "void")
                         --error $ "type not found! type: " ++ show ident ++ ", error in previous compilation steps"
    (T.TData ident args)         -> M.TData ident (map (getMono polys) args)

-- | If ident not already in env's output, morphed bind to output
-- (and all referenced binds within this bind).
-- Returns the annotated bind name.
-- TODO: Redundancy? btype and t should always be the same.
morphBind :: M.Type -> T.Bind -> EnvM Ident
morphBind expectedType b@(T.Bind (Ident _, btype) args (exp, expt)) =
    local (\env -> env { locals = Set.fromList (map fst args),
                         polys  = Map.fromList (mapTypes btype expectedType)
                       }) $ do
      -- The "new name" is used to find out if it is already marked or not.
      let name' = newFuncName expectedType b
      bindMarked <- isBindMarked (coerce name')
      -- Return with right name if already marked
      if bindMarked then return name' else do
        -- Mark so that this bind will not be processed in recursive or cyclic
        -- function calls
        markBind (coerce name')
        expt' <- getMonoFromPoly expt
        exp' <- morphExp expt' exp
        -- Get monomorphic type sof args
        args' <- mapM convertArg args
        addOutputBind $ M.Bind (coerce name', expectedType)
            args' (exp', expectedType)
        return name'

convertArg :: (Ident, T.Type) -> EnvM (Ident, M.Type)
convertArg (ident, t) = do t' <- getMonoFromPoly t
                           return (ident,  t')

-- Morphs function applications, such as EApp and EAdd
morphApp :: (M.ExpT -> M.ExpT -> M.Exp) ->  M.Type -> T.ExpT -> T.ExpT -> EnvM M.Exp
morphApp node expectedType (e1, t1) (e2, t2)= do
    t2' <- getMonoFromPoly t2
    e2' <- morphExp t2' e2
    e1' <- morphExp (M.TFun t2' expectedType) e1
    return $ node (e1', M.TFun t2' expectedType) (e2', t2')

--addOutputData :: M.Data -> EnvM ()
--addOutputData d@(M.Data (M.TData ident _) _) = modify $ Map.insert ident (Data d)

-- Gets data bind from the name of a constructor
getInputData :: Ident -> EnvM (Maybe T.Data)
getInputData ident = do env <- ask
                        return $ Map.lookup ident (dataDefs env)

-- | Expects polymorphic types in data definition to be mapped
-- in environment.
--morphData :: T.Data -> EnvM ()
--morphData (T.Data t cs) = do
--  t' <- getMonoFromPoly t
--  output <- get
--  cs' <- mapM (\(T.Inj ident t) -> do t' <- getMonoFromPoly t
--                                      return (M.Inj ident t')) cs
--  addOutputData $ M.Data t' cs'

morphCons :: M.Type -> Ident -> EnvM ()
morphCons expectedType ident = do
  maybeD <- getInputData ident
  case maybeD of
    Nothing -> error $ "identifier '" ++ show ident ++ "' not found"
    Just d -> do
      modify (\output -> Map.insert ident (Data expectedType d) output )
      -- Find the polymorphic type of cons
--      case findConsType d ident of
--        Nothing -> error "didn't find constructor"
--        Just consType -> do
--          -- Map polymorphic types
--          local (\env -> env {
--            polys = Map.fromList (mapTypes consType expectedType) }) $ do

-- TODO: detect internal errors here
--findConsType :: T.Data -> Ident -> Maybe T.Type
--findConsType (T.Data _ cs) name1 = foldl (\maybe (T.Inj name2 t) -> if name2 == name1 then Just t else maybe) Nothing cs

-- TODO: Change in tree so that these are the same.
-- Converts Lit
convertLit :: T.Lit -> M.Lit
convertLit (T.LInt v)  = M.LInt v
convertLit (T.LChar v) = M.LChar v

morphExp :: M.Type -> T.Exp -> EnvM M.Exp
morphExp expectedType exp = case exp of
  T.ELit lit -> return $ M.ELit (convertLit lit)
  -- Constructor
  T.EInj ident -> do
    return $ M.EVar ident
  T.EApp e1 e2 -> do
    morphApp M.EApp expectedType e1 e2
  T.EAdd e1 e2 -> do
    morphApp M.EAdd expectedType e1 e2
  T.EAbs ident (exp, t) -> local (\env -> env { locals = Set.insert ident (locals env) }) $ do
    t' <- getMonoFromPoly t
    morphExp t' exp
  T.ECase (exp, t) bs -> do
    t' <- getMonoFromPoly t
    bs' <- mapM morphBranch bs
    exp' <- morphExp t' exp
    return $ M.ECase (exp', t') bs'
  T.EVar ident -> do
    isLocal <- localExists ident
    if isLocal then do
      return $ M.EVar (coerce ident)
    else do
      bind <- getInputBind ident
      case bind of
        Nothing -> do
          -- This is a constructor
          morphCons expectedType ident
          return $ M.EVar ident
        Just bind' -> do
          -- New bind to process
          newBindName <- morphBind expectedType bind'
          return $ M.EVar (coerce newBindName)

  T.ELet (T.Bind {}) _ -> error "lets not possible yet"

-- Morphing case-of
morphBranch :: T.Branch -> EnvM M.Branch
morphBranch (T.Branch (p, pt) (e, et)) = do
  pt' <- getMonoFromPoly pt
  et' <- getMonoFromPoly et
  env <- ask
  (p', newLocals)  <- morphPattern pt' (locals env) p
  local (const env { locals = Set.union newLocals (locals env) }) $ do
    e' <- morphExp et' e
    return $ M.Branch (p', pt') (e', et')

-- Morphs pattern (patter -> expression), gives the newly bound local variables.
morphPattern :: M.Type -> Set.Set Ident -> T.Pattern -> EnvM (M.Pattern, Set.Set Ident)
morphPattern expectedType ls = \case
  T.PVar (ident, t) -> do t' <- getMonoFromPoly t
                          return (M.PVar (ident, t'), Set.insert ident ls)
  T.PLit (lit, t) ->   do t' <- getMonoFromPoly t
                          return (M.PLit (convertLit lit, t'), ls)
  T.PCatch -> return (M.PCatch, ls)
  -- Constructor ident
  T.PEnum ident   -> do morphCons expectedType ident
                        return (M.PEnum ident, ls)
  T.PInj ident ps -> do morphCons expectedType ident
                        let (M.TData tIdent ts) = expectedType
                        pairs <- mapM (\(pat, patT) -> morphPattern patT ls pat) (zip ps ts)
                        return (M.PInj ident (map fst pairs), Set.unions (map snd pairs))

-- | Creates a new identifier for a function with an assigned type
newFuncName :: M.Type -> T.Bind -> Ident
newFuncName t (T.Bind (ident@(Ident bindName), _) _ _) =
 if bindName == "main"
  then Ident bindName
  else newName t ident

newName :: M.Type -> Ident -> Ident
newName t (Ident str) = Ident $ str ++ "$" ++ newName' t
 where
  newName' :: M.Type -> String
  newName' (M.TLit (Ident str))     = str
  newName' (M.TFun t1 t2)           = newName' t1 ++ "_" ++ newName' t2
  newName' (M.TData (Ident str) ts) = str ++ foldl (\s t -> s ++ "." ++ newName' t) "" ts

-- Monomorphization step
monomorphize :: T.Program -> O.Program
monomorphize (T.Program defs) = removeDataTypes $ M.Program (getDefsFromOutput
    (runEnvM Map.empty (createEnv defs) monomorphize'))
 where
  monomorphize' :: EnvM ()
  monomorphize' = do
    main <- getMain
    morphBind (M.TLit $ Ident "Int") main
    return ()

-- | Runs and gives the output binds
runEnvM :: Output -> Env -> EnvM () -> Output
runEnvM o env (EnvM stateM) = snd $ runReader (runStateT stateM o) env

-- | Creates the environment based on the input binds.
createEnv :: [T.Def] -> Env
createEnv defs = Env { input    = Map.fromList bindPairs,
                       dataDefs = Map.fromList dataPairs,
                       polys    = Map.empty,
                       locals   = Set.empty }
 where
   bindPairs = (map (\b -> (getBindName b, b)) . getBindsFromDefs) defs
   dataPairs :: [(Ident, T.Data)]
   dataPairs = (foldl (\acc d@(T.Data _ cs) -> map ((,d) . getConsName) cs ++ acc) [] . getDataFromDefs) defs

-- Helper functions
-- Gets custom data declarations form defs.
getDataFromDefs :: [T.Def] -> [T.Data]
getDataFromDefs = foldl (\bs -> \case
                                   T.DBind _ -> bs
                                   T.DData d -> d:bs) []

getConsName :: T.Inj -> Ident
getConsName (T.Inj ident _) = ident

getBindsFromDefs :: [T.Def] -> [T.Bind]
getBindsFromDefs = foldl (\bs -> \case
                                   T.DBind b -> b:bs
                                   T.DData _ -> bs) []

getDefsFromOutput :: Output -> [M.Def]
getDefsFromOutput o =
  map M.DBind binds ++
  (map (M.DData . snd) . Map.toList) (createNewData dataInput Map.empty)
 where
   (binds, dataInput) = splitBindsAndData o

-- | Splits the output into binds and data declaration components (used in createNewData)
splitBindsAndData :: Output -> ([M.Bind], [(Ident, M.Type, T.Data)])
splitBindsAndData output = foldl
  (\(oBinds, oData) (ident, o) -> case o of
                                    Incomplete -> error "internal bug in monomorphizer"
                                    Complete b   -> (b:oBinds, oData)
                                    Data     t d -> (oBinds, (ident, t, d):oData))
  ([], [])
  (Map.toList output)

-- | Converts all found constructors to monomorphic data declarations.
createNewData :: [(Ident, M.Type, T.Data)] -> Map.Map Ident M.Data -> Map.Map Ident M.Data
createNewData []                                      o = o
createNewData ((consIdent, consType, polyData):input) o =
  createNewData input $
    Map.insertWith (\_ (M.Data _ cs) -> M.Data newDataType (newCons:cs))
      newDataName (M.Data newDataType [newCons]) o
 where
  T.Data (T.TData polyDataIdent _) _ = polyData
  newDataType = getDataType consType
  newDataName = newName newDataType polyDataIdent
  newCons     = M.Inj consIdent consType

getDataType :: M.Type -> M.Type
getDataType       (M.TFun t1 t2) = getDataType t2
getDataType tData@(M.TData _ _)  = tData
getDataType _                    = error "???"

-- | Converts all found constructors to monomorphic data declarations.
--               cons->data              process    data.name -> data
--createNewData :: [(Ident, M.Type, T.Data)] -> Map.Map Ident M.Data -> EnvM (Map.Map Ident M.Data)
--createNewData []     o = return o
--createNewData ((ident, expectedType, T.Data dt pcs):cs) o = case dt of
--  T.TData dIdent _ -> do
--    let newCons = M.Inj (newName expectedType ident) expectedType
--    case Map.lookup dIdent o of
--      Nothing -> do
--        createNewData cs $ Map.insert ident (M.Data (M.TLit $ Ident "void") [newCons]) o
--      Just _ -> do
--        createNewData cs $ Map.adjust (\(M.Data _ pcs') ->
--          M.Data expectedType (newCons : pcs')) ident o
--  _ -> error "internal bug in monomorphizer"

getBindName :: T.Bind -> Ident
getBindName (T.Bind (ident, _) _ _) = ident

