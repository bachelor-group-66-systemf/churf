{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use traverse_" #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeChecker.HM  where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor        (bimap, second)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Map              (Map)
import qualified Data.Map              as M

import           Grammar.Abs
import           Grammar.Print
import qualified TypeChecker.HMIr      as T

type Infer = StateT Ctx (ExceptT String Identity)
type Error = String

data Ctx = Ctx { constr :: Map Type Type
               , vars   :: Map Ident Type
               , sigs   :: Map Ident Type
               , frsh   :: Char }
    deriving Show

runC :: Ctx -> Infer a -> Either String (a, Ctx)
runC c = runIdentity . runExceptT . flip runStateT c

run :: Infer a -> Either String a
run = runIdentity . runExceptT . flip evalStateT initC

initC :: Ctx
initC = Ctx M.empty M.empty M.empty 'a'

typecheck :: Program -> Either Error T.Program
typecheck = run . inferPrg

inferPrg :: Program -> Infer T.Program
inferPrg (Program bs) = do
    traverse (\(Bind n t _ _ _) -> insertSig n t) bs
    bs' <- mapM inferBind bs
    return $ T.Program bs'

inferBind :: Bind -> Infer T.Bind
inferBind (Bind i t _ params rhs) = do
    (t',e') <- inferExp (makeLambda rhs (reverse params))
    when (t /= t') (throwError . unwords $ [ "Signature of function"
                                           , show i
                                           , "with type:"
                                           , show t
                                           , "does not match inferred type"
                                           , show t'
                                           , "of expression:"
                                           , show e'])
    return $ T.Bind (t,i) [] e'

makeLambda :: Exp -> [Ident] -> Exp
makeLambda = foldl (flip EAbs)

inferExp :: Exp -> Infer (Type, T.Exp)
inferExp e = do
    (t, e') <- inferExp' e
    t'' <- solveConstraints t
    return (t'', replaceType t'' e')

  where
    inferExp' :: Exp -> Infer (Type, T.Exp)
    inferExp' = \case
        EAnn e t -> do
            (t',e') <- inferExp' e
            t'' <- solveConstraints t'
            when (t'' /= t) (throwError "Annotated type and inferred type don't match")
            return (t', e')
        EInt i -> return (int, T.EInt int i)
        EId i -> (\t -> (t, T.EId t i)) <$> lookupVar i
        EAdd e1 e2 -> do
            insertSig "+" (TArr int (TArr int int))
            inferExp' (EApp (EApp (EId "+") e1) e2)
        EApp e1 e2 -> do
            (t1, e1') <- inferExp' e1
            (t2, e2') <- inferExp' e2
            fr <- fresh
            addConstraint t1 (TArr t2 fr)
            return (fr, T.EApp fr e1' e2')
        EAbs name e -> do
            fr <- fresh
            insertVar name fr
            (ret_t,e') <- inferExp' e
            t <- solveConstraints (TArr fr ret_t)
            return (t, T.EAbs t name e')
        ELet name e1 e2 -> error "Let expression not implemented yet"

replaceType :: Type -> T.Exp -> T.Exp
replaceType t = \case
        T.EInt _ i -> T.EInt t i
        T.EId _ i -> T.EId t i
        T.EAdd _ e1 e2 -> T.EAdd t e1 e2
        T.EApp _ e1 e2 -> T.EApp t e1 e2
        T.EAbs _ name e -> T.EAbs t name e
        T.ELet _ name e1 e2 -> T.ELet t name e1 e2

isInt :: Type -> Bool
isInt (TMono "Int") = True
isInt _             = False

lookupVar :: Ident -> Infer Type
lookupVar i = do
    st <- get
    case M.lookup i (vars st) of
      Just t -> return t
      Nothing -> case M.lookup i (sigs st) of
        Just t  -> return t
        Nothing -> throwError $ "Unbound variable or function" ++ printTree i

insertVar :: Ident -> Type -> Infer ()
insertVar s t = modify ( \st -> st { vars = M.insert s t (vars st) } )

insertSig :: Ident -> Type -> Infer ()
insertSig s t = modify ( \st -> st { sigs = M.insert s t (sigs st) } )

-- | Generate a new fresh variable and increment the state
fresh :: Infer Type
fresh = do
    chr <- gets frsh
    modify (\st -> st { frsh = succ chr })
    return $ TPol (Ident [chr])

-- | Adds a constraint to the constraint set.
-- i.e: a = int -> b
--      b = int
-- thus when solving constraints it must be the case that
--      a = int -> int
addConstraint :: Type -> Type -> Infer ()
addConstraint t1 t2 = do
    modify (\st -> st { constr = M.insert t1 t2 (constr st) })

-- | Given a type, solve the constraints and figure out the type that should be assigned to it.
solveConstraints :: Type -> Infer Type
solveConstraints t = do
    c <- gets constr
    v <- gets vars
    xs <- solveAll (M.toList c)
    modify (\st -> st { constr = M.fromList xs })
    return $ subst t xs

-- | Substitute
subst :: Type -> [(Type, Type)] -> Type
subst t []                = t
subst (TArr t1 t2) (x:xs) = subst (TArr (replace x t1) (replace x t2)) xs
subst t (x:xs)            = subst (replace x t) xs

-- | Given a set of constraints run the replacement on all of them, producing a new set of
--  replacements.
--  https://youtu.be/trmq3wYcUxU - good video for explanation
solveAll :: [(Type, Type)] -> Infer [(Type, Type)]
solveAll [] = return []
solveAll (x:xs) = case x of
    (TArr t1 t2, TArr t3 t4) -> solveAll $ (t1,t3) : (t2,t4) : xs
    (TArr t1 t2, b)          -> fmap ((b, TArr t1 t2) :) $ solveAll $ solve (b, TArr t1 t2) xs
    (a, TArr t1 t2)          -> fmap ((a, TArr t1 t2) :) $ solveAll $ solve (a, TArr t1 t2) xs
    (TMono a, TPol b)        -> fmap ((TPol b, TMono a) :) $ solveAll $ solve (TPol b, TMono a) xs
    (TPol a, TMono b)        -> fmap ((TPol a, TMono b) :) $ solveAll $ solve (TPol a, TMono b) xs
    (TPol a, TPol b)         -> fmap ((TPol a, TPol b) :) $ solveAll $ solve (TPol a, TPol b) xs
    (TMono a, TMono b)       -> if a == b then solveAll xs else throwError "Can't unify types"

solve :: (Type, Type) -> [(Type, Type)] -> [(Type, Type)]
solve x = map (both (replace x))

-- | Given a constraint (type, type) and a type, if the constraint matches the input
--   replace with the constrained type
replace :: (Type, Type) -> Type -> Type
replace a (TArr t1 t2) = TArr (replace a t1) (replace a t2)
replace (a,b) c        = if a==c then b else c

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f

int = TMono "Int"
