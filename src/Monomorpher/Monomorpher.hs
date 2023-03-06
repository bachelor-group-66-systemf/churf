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

module Monomorpher.Monomorpher (monomorphize) where

import qualified TypeChecker.TypeCheckerIr as T
import qualified Monomorpher.MonomorpherIr as M

import           Grammar.Abs   (Ident)

import Control.Monad.State (MonadState (get), State, gets, modify, execState)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

-- | The environment of computations in this module.
data Env = Env { -- | All binds in the program.
                 input  :: Map.Map Ident T.Bind,
                 -- | The monomorphized binds.
                 output :: Map.Map Ident M.Bind,
                 -- | Maps polymorphic identifiers with concrete types.
                 polys  :: Map.Map Ident M.Type,
                 -- | Local variables, not necessary if id's are annotated based
                 -- on if they are local or global.
                 locals :: Set.Set Ident
               }

-- | State Monad wrapper for "Env".
type EnvM a = State Env a

-- TODO: use fromList
-- | Creates the environment based on the input binds.
createEnv :: [T.Bind] -> Env
createEnv binds = Env { input  = Map.fromList kvPairs, 
                        output = Map.empty,
                        polys  = Map.empty,
                        locals = Set.empty }
 where
   kvPairs :: [(Ident, T.Bind)]
   kvPairs = map (\b@(T.Bind (ident, _) _ _) -> (ident, b)) binds

-- | Functions to add, clear and get whether id is a local variable.
addLocal :: Ident -> EnvM ()
addLocal ident = modify (\env -> env { locals = Set.insert ident (locals env) })

clearLocal :: EnvM ()
clearLocal = modify (\env -> env { locals = Set.empty })

localExists :: Ident -> EnvM Bool
localExists ident = do env <- get
                       return $ Set.member ident (locals env)

-- | Gets a polymorphic bind from an id.
getPolymorphic :: Ident -> EnvM (Maybe T.Bind)
getPolymorphic ident = gets (Map.lookup ident . input)

-- | Add monomorphic function derived from a polymorphic one, to env.
addMonomorphic :: M.Bind -> EnvM ()
addMonomorphic b@(M.Bind (ident, _) _ _) = modify 
  (\env -> env { output = Map.insert ident b (output env) })

-- | Checks whether or not an ident is added to output binds.
isOutputted :: Ident -> EnvM Bool
isOutputted ident = do env <- get
                       return $ Map.member ident (output env)

-- | Finds main bind
getMain :: EnvM T.Bind
getMain = gets (\env -> fromJust $ Map.lookup (T.Ident "main") (input env))

-- | Add polymorphic -> monomorphic type bindings regardless of bind.
-- The structue of the types should be the same, map them.
addPolyMap :: M.Type -> T.Bind -> EnvM ()
addPolyMap t1 (T.Bind (_, t2) _ _) = modify modFunc
 where
   modFunc env = env { polys = newPolys (polys env) }
   newPolys oldPolys = Map.union oldPolys (Map.fromList (mapTypes t2 t1))

-- | Gets the monomorphic type of a polymorphic type in the current context.
getMono :: T.Type -> EnvM M.Type
getMono t = do env <- get
               return $ getMono' (polys env) t
 where
  getMono' :: Map.Map Ident M.Type -> T.Type -> M.Type
  getMono' polys t = case t of
    (T.TMono ident) -> M.TMono ident
    (T.TArr t1 t2)  -> M.TArr
        (getMono' polys t1) (getMono' polys t2)
    (T.TPol ident) -> case Map.lookup ident polys of
                         Just concrete -> concrete
                         Nothing       -> error "type not found!"

-- NOTE: could make this function more optimized
-- | Makes a kv pair list of poly to concrete mappings, throws runtime
-- error when encountering different structures between the two arguments.
mapTypes :: T.Type -> M.Type -> [(Ident, M.Type)]
mapTypes (T.TMono _)     (M.TMono _)       = []
mapTypes (T.TPol i1)      tm               = [(i1, tm)]
mapTypes (T.TArr pt1 pt2) (M.TArr mt1 mt2) = mapTypes pt1 mt1 ++ 
                                             mapTypes pt2 mt2
mapTypes _ _ = error "structure of types not the same!"

-- | If ident not already in env's output, morphed bind to output
-- (and all referenced binds within this bind).
morphBind :: M.Type -> T.Bind -> EnvM ()
morphBind expectedType b@(T.Bind (ident, _) _ exp) = do
  outputted <- isOutputted ident
  if outputted then
    -- Don't add anything!
    return ()
  else do
    -- Add processed bind!
    addPolyMap expectedType b
    exp' <- morphExp expectedType exp
    addMonomorphic $ M.Bind (ident, expectedType) [] exp'

-- Get type of expression
getExpType :: T.Exp -> T.Type
getExpType (T.EId  (_, t)) = t
getExpType (T.ELit t _)    = t
getExpType (T.EApp t _ _)  = t
getExpType (T.EAdd t _ _)  = t
getExpType (T.EAbs t _ _)  = t
getExpType (T.ELet _ _)    = error "Lets not allowed🛑👮"

morphExp :: M.Type -> T.Exp -> EnvM M.Exp
morphExp expectedType exp = case exp of
  T.ELit t lit   -> do t' <- getMono t  -- These steps are abundant
                       return $ M.ELit t' lit
  T.EApp _ e1 e2 -> do t2  <- getMono $ getExpType e2
                       e2' <- morphExp t2 e2
                       t1  <- getMono $ getExpType e1
                       e1' <- morphExp t1 e1
                       return $ M.EApp expectedType e1' e2'
  T.EAdd _ e1 e2 -> do t2  <- getMono $ getExpType e2
                       e2' <- morphExp t2 e2
                       t1  <- getMono $ getExpType e1
                       e1' <- morphExp t1 e1
                       return $ M.EApp expectedType e1' e2'
  -- Add local vars to locals
  T.EAbs _ (ident, _) e -> do let (M.TArr _ t) = expectedType
                              addLocal ident
                              morphExp t e

  T.EId (ident, t) ->      do maybeLocal <- localExists ident
                              if maybeLocal then do
                                t' <- getMono t
                                return $ M.EId (ident, t')
                              else do
                                clearLocal
                                bind <- getPolymorphic ident
                                case bind of
                                  Nothing    -> error "Wowzers!"
                                  Just bind' -> do
                                    t' <- getMono t
                                    morphBind t' bind'
                                    return $ M.EId (ident, t')

  T.ELet (T.Bind {}) _ -> error "Lets not possible yet."

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


