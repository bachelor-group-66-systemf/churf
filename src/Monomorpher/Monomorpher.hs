-- | For now, converts polymorphic functions to concrete ones based on usage.
-- Assumes lambdas are lifted.
-- This step of compilation is as follows:
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
-- When an external bind is encountered (EId), it is checked whether it is
-- monomorphic or polymorphic. If monomorphic, nothing further is evaluated.
-- If polymorphic, the bind transformer function is called on this with the
-- expected type in this context. The result of this computation (a monomorphic 
-- bind) is added to the resulting set of binds.

module Monomorpher.Monomorpher (monomorphize) where

import qualified TypeChecker.TypeCheckerIr as T
import qualified Monomorpher.MonomorpherIr as M

import           Grammar.Abs   (Ident)

import Control.Monad.State (MonadState (get, put), State, gets, modify)
import qualified Data.Map as Map
import Data.Foldable (find)

-- | The environment of computations in this module.
data Env = Env { -- | All binds in the program.
                 input  :: Map.Map Ident T.Bind,
                 -- | The monomorphized binds.
                 output :: [M.Bind],
                 -- | Maps polymorphic identifiers with concrete types.
                 polys  :: Map.Map Ident M.Type
               }

-- | State Monad wrapper for "Env".
type EnvM a = State Env a

-- TODO: use fromList
-- | Creates the environment based on the input binds.
createEnv :: [T.Bind] -> Env
createEnv binds = Env { input = Map.fromList kvPairs }
 where
   kvPairs :: [(Ident, T.Bind)]
   kvPairs = map (\b@(T.Bind (ident, _) _ _) -> (ident, b)) binds

-- | Gets a polymorphic bind from an id.
getPolymorphic :: T.Id -> EnvM (Maybe T.Bind)
getPolymorphic (ident, _) = gets (Map.lookup ident . input)

-- | Add monomorphic function derived from a polymorphic one, to env.
addMonomorphic :: M.Bind -> EnvM ()
addMonomorphic b = modify (\env -> env { output = b:(output env) })

-- | Add polymorphic -> monomorphic type bindings regardless of bind.
addPolyMap :: M.Type -> T.Bind -> EnvM ()
addPolyMap = undefined

--morphBind :: M.Type -> T.Bind -> EnvM M.Bind
--morphBind expectedType (T.Bind (ident, t) _ exp) = do
--  exp' <- morphExp expectedType exp
--  return $ M.Bind (ident, expectedType) [] exp'
--
---- | Monomorphize an expression.
--morphExp :: M.Type -> T.Exp -> EnvM M.Exp
--morphExp expectedType exp = case exp of
--  T.EApp t e1 e2 -> do  
--    e1' <- morphExp expectedType e1
--    e2' <- morphExp t1 e2
--    return $ M.EApp expectedType e1' e2'
--  T.EAdd t e1 e2 -> do e1' <- morphExp e1
--                       e2' <- morphExp e2
--                       return $ M.EAdd t e1' e2'
--  T.EId  id ->undefined
--  T.ELit t lit ->undefined
--  T.ELet bind e ->undefined
--  -- Special case at bind level
--  T.EAbs t id e -> error "Passing lambda lifter, this is not possible."

-- | Does the monomorphization.
monomorphize :: T.Program -> M.Program
monomorphize (T.Program binds) = undefined
 where
  monomorphize' :: EnvM M.Program
  monomorphize' = do
    put $ createEnv binds
    -- TODO: complete
    return $ M.Program []

---- | Add functions (including polymorphic ones) to global environment.
--addBind :: Env -> Def -> Err Env
--addBind env (DDef ident identArgs closure) = envAdd env ident (foldl (flip EAbs) closure identArgs)

