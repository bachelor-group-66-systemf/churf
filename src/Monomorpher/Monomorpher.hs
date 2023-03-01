-- | For now, converts polymorphic functions to concrete ones based on usage.
-- Assumes lambdas are lifted.
module Monomorpher.Monomorpher (monomorphize) where

import qualified TypeChecker.TypeCheckerIr as T
import TypeChecker.TypeCheckerIr (Id)

import qualified Monomorpher.MonomorpherIr as M
import Control.Monad.State (MonadState (get, put), State)
import qualified Data.Map as Map

data Env = Env { input :: Map.Map Id T.Bind, output :: Map.Map Id M.Bind }
-- | Monad containing the, outputted
type EnvM a = State Env a

-- | Creates the environment based on the input binds.
createEnv :: [T.Bind] -> Env
createEnv binds = Env { input = foldl createEnv' Map.empty binds, output = Map.empty }
 where
   createEnv' ins b@(T.Bind name args exp) = Map.insert name b ins

-- | Does the monomorphization.
monomorphize :: T.Program -> M.Program
monomorphize = undefined

-- | Monomorphize an expression.
--morphExp :: T.Exp -> EnvM M.Exp
--morphExp exp = case exp of
--  T.EId id -> return $ M.EId id


---- | Add functions (including polymorphic ones) to global environment.
--addBind :: Env -> Def -> Err Env
--addBind env (DDef ident identArgs closure) = envAdd env ident (foldl (flip EAbs) closure identArgs)

