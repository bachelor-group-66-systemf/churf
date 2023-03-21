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

module Monomorpher.Monomorpher (monomorphize, morphExp, morphBind) where

import qualified TypeChecker.TypeCheckerIr as T
import qualified Monomorpher.MonomorpherIr as M

import           Grammar.Abs   (Ident (Ident))

import Control.Monad.State (MonadState (get), State, gets, modify, execState)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Debug.Trace

-- | The environment of computations in this module.
data Env = Env { -- | All binds in the program.
                 input  :: Map.Map Ident T.Bind,
                 -- | The monomorphized binds.
                 output :: Map.Map Ident M.Bind,
                 -- | Maps polymorphic identifiers with concrete types.
                 polys  :: Map.Map Ident M.Type,
                 -- | Local variables, not necessary if id's are annotated based
                 -- on if they are local or global.
                 locals :: Set.Set Ident,
                 -- | The identifier of the current function.
                 currentFunc :: Ident
               } deriving (Show)

-- | State Monad wrapper for "Env".
type EnvM a = State Env a

-- | Creates the environment based on the input binds.
createEnv :: [T.Bind] -> Env
createEnv binds = Env { input  = Map.fromList kvPairs, 
                        output = Map.empty,
                        polys  = Map.empty,
                        locals = Set.empty,
                        currentFunc = Ident "main" }
 where
   kvPairs :: [(Ident, T.Bind)]
   kvPairs = map (\b@(T.Bind (ident, _) _ _) -> (ident, b)) binds

-- | Functions to add, clear and get whether id is a local variable.
addLocal :: Ident -> EnvM ()
addLocal ident = modify (\env -> env { locals = Set.insert ident (locals env) })

addLocals :: [Ident] -> EnvM ()
addLocals idents = modify (\env -> 
  env { locals = Set.fromList idents `Set.union` locals env })

clearLocals :: EnvM ()
clearLocals = modify (\env -> env { locals = Set.empty })

localExists :: Ident -> EnvM Bool
localExists ident = do env <- get
                       return $ Set.member ident (locals env)

-- | Gets whether ident is current function.
isCurrentFunc :: Ident -> EnvM Bool
isCurrentFunc ident = do env <- get
                         return $ ident == currentFunc env

-- | Gets a polymorphic bind from an id.
getInputBind :: Ident -> EnvM (Maybe T.Bind)
getInputBind ident = gets (Map.lookup ident . input)

-- | Add monomorphic function derived from a polymorphic one, to env.
addOutputBind :: M.Bind -> EnvM ()
addOutputBind b@(M.Bind (ident, _) _ _) = modify 
  (\env -> env { output = Map.insert ident b (output env) })

-- | Checks whether or not an ident is added to output binds.
isBindOutputted :: Ident -> EnvM Bool
isBindOutputted ident = do env <- get
                           return $ Map.member ident (output env)

-- | Finds main bind
getMain :: EnvM T.Bind
getMain = gets (\env -> fromJust $ Map.lookup (T.Ident "main") (input env))

-- | Add polymorphic -> monomorphic type bindings regardless of bind.
-- The structue of the types should be the same.
mapTypesInBind :: M.Type -> T.Bind -> EnvM ()
mapTypesInBind t1 (T.Bind (_, t2) _ _) = modify modFunc
 where
   modFunc env = env { polys = newPolys (polys env) }
   newPolys oldPolys = Map.union oldPolys (Map.fromList (mapTypes t2 t1))

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
getMonoFromPoly t = do env <- get
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
                           "type not found! type: " ++ show ident

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
morphBind :: M.Type -> T.Bind -> EnvM ()
morphBind expectedType b@(T.Bind (Ident name, _) args exp) = do
  outputted <- isBindOutputted (Ident name)
  if outputted then
    -- Don't add anything!
    return ()
  else do
    -- Add processed bind!
    addLocals $ map fst args  -- Add all the local variables
    mapTypesInBind expectedType b
    exp' <- morphExp expectedType exp
    addOutputBind $ M.Bind (newName expectedType b, expectedType) [] exp'

morphExp :: M.Type -> T.Exp -> EnvM M.Exp
morphExp expectedType exp = case exp of
  T.ELit t lit   -> do t' <- getMonoFromPoly t  -- These steps are abundant
                       return $ M.ELit t' lit
  T.EApp _ e1 e2 -> do t2  <- getMonoFromPoly $ getExpType e2
                       e2' <- morphExp t2 e2
                       t1  <- getMonoFromPoly $ getExpType e1
                       e1' <- morphExp t1 e1
                       return $ M.EApp expectedType e1' e2'
  T.EAdd _ e1 e2 -> do t2  <- getMonoFromPoly $ getExpType e2
                       e2' <- morphExp t2 e2
                       t1  <- getMonoFromPoly $ getExpType e1
                       e1' <- morphExp t1 e1
                       return $ M.EAdd expectedType e1' e2'
  -- Add local vars to locals, this will never be called after the lambda lifter
  T.EAbs _ (ident, _) e -> do let (M.TArr _ t) = expectedType
                              error "EAbs found in Monomorpher, should not be possible"
                              addLocal ident
                              morphExp t e

  T.EId (ident@(Ident istr), t) -> do 
    maybeLocal <- localExists ident
    if maybeLocal then do
      t' <- getMonoFromPoly t
      return $ M.EId (ident, t')
    else do
      clearLocals
      bind <- getInputBind ident
      case bind of
        Nothing -> 
          error $ "bind of name: " ++ istr ++ " not found"
        Just bind' -> do
          maybeCurrentFunc <- isCurrentFunc ident
          t' <- getMonoFromPoly t
          if maybeCurrentFunc then -- Recursive call?
            return ()
          else
            morphBind t' bind'
          return $ M.EId (ident, t')

  T.ELet (T.Bind {}) _ -> error "lets not possible yet"

-- Creates a new identifier for a function with an assigned type
newName :: M.Type -> T.Bind -> Ident
newName t (T.Bind (Ident bindName, _) _ _) = Ident (bindName ++ "$" ++ newName' t)
 where
  newName' :: M.Type -> String
  newName' (M.TMono (Ident str)) = str
  newName' (M.TArr t1 t2)        = newName' t1 ++ "_" ++ newName' t2

-- TODO: make sure that monomorphic binds are not processed again
-- | Does the monomorphization.
monomorphize :: T.Program -> M.Program
monomorphize (T.Program binds) = M.Program $ (map snd . Map.toList) outputMap
 where
  outputMap :: Map.Map Ident M.Bind
  outputMap = output $ execState monomorphize' (createEnv binds)

  monomorphize' :: EnvM ()
  monomorphize' = do
    main <- getMain
    morphBind (M.TMono $ M.Ident "Int") main

