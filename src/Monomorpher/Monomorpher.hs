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

module Monomorpher.Monomorpher (monomorphize, morphExp, morphBind) where

import qualified TypeChecker.TypeCheckerIr as T
import qualified Monomorpher.MonomorpherIr as M

import           Grammar.Abs   (Ident (Ident))

import Debug.Trace
import Control.Monad.State (MonadState, gets, modify, StateT (runStateT))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Control.Monad.Reader (Reader, MonadReader (local, ask), asks, runReader)

-- | State Monad wrapper for "Env".
newtype EnvM a = EnvM (StateT Output (Reader Env) a)
  deriving (Functor, Applicative, Monad, MonadState Output, MonadReader Env)

type Output = Map.Map Ident Outputted
-- When a bind is being processed, it is Incomplete in the state, also
-- called marked.
data Outputted = Incomplete | Complete M.Bind

-- Static environment
data Env = Env {
  -- | All binds in the program.
  input :: Map.Map Ident T.Bind,
  -- | Maps polymorphic identifiers with concrete types.
  polys :: Map.Map Ident M.Type,
  -- | Local variables
  locals :: Set.Set Ident
}

runEnvM :: Output -> Env -> EnvM () -> Output
runEnvM o env (EnvM stateM) = snd $ runReader (runStateT stateM o) env



-- | Creates the environment based on the input binds.
createEnv :: [T.Bind] -> Env
createEnv binds = Env { input  = Map.fromList kvPairs, 
                        polys  = Map.empty,
                        locals = Set.empty }
 where
   kvPairs :: [(Ident, T.Bind)]
   kvPairs = map (\b@(T.Bind (ident, _) _ _) -> (ident, b)) binds

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
mapTypes (T.TMono _)      (M.TMono _)      = []
mapTypes (T.TPol i1)      tm               = [(i1, tm)]
mapTypes (T.TArr pt1 pt2) (M.TArr mt1 mt2) = mapTypes pt1 mt1 ++ 
                                             mapTypes pt2 mt2
mapTypes _ _ = error "structure of types not the same!"

-- | Gets the mapped monomorphic type of a polymorphic type in the current context.
getMonoFromPoly :: T.Type -> EnvM M.Type
getMonoFromPoly t = do env <- ask
                       return $ getMono (polys env) t
 where
  getMono :: Map.Map Ident M.Type -> T.Type -> M.Type
  getMono polys t = case t of
    (T.TMono ident) -> M.TMono ident
    (T.TArr t1 t2)  -> M.TArr
        (getMono polys t1) (getMono polys t2)
    (T.TPol ident) -> case Map.lookup ident polys of
                         Just concrete -> concrete
                         Nothing       -> error $ 
                           "type not found! type: " ++ show ident ++ ", error in previous compilation steps"

-- Get type of expression
getExpType :: T.Exp -> T.Type
getExpType (T.EId  (_, t)) = t
getExpType (T.ELit t _)    = t
getExpType (T.EApp t _ _)  = t
getExpType (T.EAdd t _ _)  = t
getExpType (T.EAbs t _ _)  = t
getExpType (T.ELet _ _)    = error "lets not allowed🛑👮"

-- | If ident not already in env's output, morphed bind to output
-- (and all referenced binds within this bind).
-- Returns the annotated bind name.
morphBind :: M.Type -> T.Bind -> EnvM Ident
morphBind expectedType b@(T.Bind (Ident _, btype) args exp) =
    local (\env -> env { locals = Set.fromList (map fst args),
                         polys  = Map.fromList (mapTypes btype expectedType)
                       }) $ do
      -- The "new name" is used to find out if it is already marked or not.
      let name' = newName expectedType b
      bindMarked <- isBindMarked name'
      -- Return with right name if already marked
      if bindMarked then return name' else do
        -- Mark so that this bind will not be processed in recursive or cyclic 
        -- function calls
        markBind name' 
        exp' <- morphExp expectedType exp
        addOutputBind $ M.Bind (name', expectedType) [] exp'
        return name'

-- Morphs function applications, such as EApp and EAdd
morphApp :: M.Type -> T.Exp -> T.Exp -> EnvM M.Exp
morphApp expectedType e1 e2 = do
    t2  <- getMonoFromPoly $ getExpType e2 -- TODO: make better helper
    e2' <- morphExp t2 e2
    e1' <- morphExp (M.TArr t2 expectedType) e1
    return $ M.EApp (M.TArr t2 expectedType) e1' e2'
    --t2  <- getMonoFromPoly $ getExpType e2
    --e2' <- morphExp t2 e2
    --t1  <- getMonoFromPoly $ getExpType e1
    --e1' <- morphExp t1 e1
    --return $ M.EApp expectedType e1' e2'

morphExp :: M.Type -> T.Exp -> EnvM M.Exp
morphExp expectedType exp = case exp of
  T.ELit t lit   -> do 
    t' <- getMonoFromPoly t  -- These steps are abundant
    return $ M.ELit t' lit
  T.EApp _ e1 e2 -> do
    morphApp expectedType e1 e2
  T.EAdd _ e1 e2 -> do
    morphApp expectedType e1 e2
  T.EAbs _ (_, _) _ -> do
    error "EAbs found in Monomorpher, should not be possible"
  T.EId (ident@(Ident istr), t) -> do
    isLocal <- localExists ident
    t' <- getMonoFromPoly t
    if isLocal then do
      return $ M.EId (ident, t')
    else do
      bind <- getInputBind ident
      case bind of
        Nothing -> 
          error $ "bind of name: " ++ istr ++ " not found, bug in previous compilation steps"
        Just bind' -> do
          -- New bind to process
          newBindName <- morphBind t' bind'
          return $ M.EId (newBindName, t')

  T.ELet (T.Bind {}) _ -> error "lets not possible yet"

-- Creates a new identifier for a function with an assigned type
newName :: M.Type -> T.Bind -> Ident
newName t (T.Bind (Ident bindName, _) _ _) = Ident (bindName ++ "$" ++ newName' t)
 where
  newName' :: M.Type -> String
  newName' (M.TMono (Ident str)) = str
  newName' (M.TArr t1 t2)        = newName' t1 ++ "_" ++ newName' t2

-- Monomorphization step
monomorphize :: T.Program -> M.Program
monomorphize (T.Program binds) = M.Program $ getBindsFromOutput 
    (runEnvM Map.empty (createEnv binds) monomorphize')
 where
  monomorphize' :: EnvM ()
  monomorphize' = do
    main <- getMain
    morphBind (M.TMono $ M.Ident "Int") main
    return ()

getBindsFromOutput :: Output -> [M.Bind]
getBindsFromOutput outputMap = (map snd . Map.toList) $ fmap 
                                 (\case
                                    Incomplete -> error ""
                                    Complete b -> b ) 
                                  outputMap

