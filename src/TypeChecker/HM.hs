{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use traverse_" #-}

module TypeChecker.HM (typecheck) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor        (second)
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

run :: Infer a -> Either String a
run = runIdentity . runExceptT . flip evalStateT initC

int = TMono "Int"

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
    (t',e') <- inferExp (makeLambda (reverse params) rhs)
    addConstraint t t'
    -- when (t /= t') (throwError $ "Signature of function" ++ printTree i ++ "does not match inferred type of expression: " ++ printTree e')
    return $ T.Bind (t,i) [] e'

makeLambda :: [Ident] -> Exp -> Exp
makeLambda xs e = foldl (flip EAbs) e xs

inferExp :: Exp -> Infer (Type, T.Exp)
inferExp = \case
    EAnn e t -> do
        (t',e') <- inferExp e
        when (t' /= t) (throwError "Annotated type and inferred type don't match")
        return (t', e')
    EInt i -> return (int, T.EInt int i)
    EId i -> (\t -> (t, T.EId t i)) <$> lookupVar i
    EAdd e1 e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        unless (isInt t1 && isInt t2) (throwError "Can not add non-ints")
        return (int,T.EAdd int e1' e2')
    EApp e1 e2 -> do
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        fr <- fresh
        addConstraint t1 (TArr t2 fr)
        return (fr, T.EApp fr e1' e2')
    EAbs name e -> do
        fr <- fresh
        insertVar name fr
        (ret_t,e') <- inferExp e
        t <- solveConstraints (TArr fr ret_t)
        return (t, T.EAbs t name e')
    ELet name e1 e2 -> do
        fr <- fresh
        insertVar name fr
        (t1, e1') <- inferExp e1
        (t2, e2') <- inferExp e2
        ret_t <- solveConstraints t1
        return (ret_t, T.ELet ret_t name e1' e2')


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


fresh :: Infer Type
fresh = do
    chr <- gets frsh
    modify (\st -> st { frsh = succ chr })
    return $ TPol (Ident [chr])

addConstraint :: Type -> Type -> Infer ()
addConstraint t1 t2 = do
    when (t2 `contains` t1) (throwError $ "Can't match type " ++ printTree t1 ++ " with " ++ printTree t2)
    modify (\st -> st { constr = M.insert t1 t2 (constr st) })

contains :: Type -> Type -> Bool
contains (TArr t1 t2) b      = t1 `contains` b || t2 `contains` b
contains (TMono a) (TMono b) = False
contains a b                 = a == b

solveConstraints :: Type -> Infer Type
solveConstraints t = do
    c <- gets constr
    v <- gets vars
    subst t <$> solveAll (M.toList c)

subst :: Type -> [(Type, Type)] -> Type
subst t []                = t
subst (TArr t1 t2) (x:xs) = subst (TArr (replace x t1) (replace x t2)) xs
subst t (x:xs)            = subst (replace x t) xs

solveAll :: [(Type, Type)] -> Infer [(Type, Type)]
solveAll [] = return []
solveAll (x:xs) = case x of
    (TArr t1 t2, TArr t3 t4) -> solveAll $ (t1,t3) : (t2,t4) : xs
    (TArr t1 t2, b)          -> fmap ((b, TArr t1 t2) :) $ solveAll $ solve (b, TArr t1 t2) xs
    (a, TArr t1 t2)          -> fmap ((a, TArr t1 t2) :) $ solveAll $ solve (a, TArr t1 t2) xs
    (TMono a, TPol b)        -> fmap ((TPol b, TMono a) :) $ solveAll $ solve (TPol b, TMono a) xs
    (TPol a, TMono b)        -> fmap ((TPol a, TMono a) :) $ solveAll $ solve (TPol a, TMono b) xs
    (TMono a, TMono b)       -> if a == b then solveAll xs else throwError "Can't unify types"
    (TPol a, TPol b)         -> fmap ((TPol a, TPol b) :) $ solveAll $ solve (TPol a, TPol b) xs

solve :: (Type, Type) -> [(Type, Type)] -> [(Type, Type)]
solve x = map (second (replace x))

replace :: (Type, Type) -> Type -> Type
replace a (TArr t1 t2) = TArr (replace a t1) (replace a t2)
replace (a,b) c        = if a==c then b else c

-- Known bugs
-- (x : a) + 3 type checks
