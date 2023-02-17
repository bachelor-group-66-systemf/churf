{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.TypeChecker where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT)
import Control.Monad.State qualified as St
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map (Map)
import Data.Map qualified as M
import TypeChecker.TypeCheckerIr

data Ctx = Ctx
    { vars :: Map Integer Type
    , sigs :: Map Ident (RBind, Maybe Type)
    , nextFresh :: Int
    }
    deriving (Show)

-- Perhaps swap over to reader monad instead for vars and sigs.
type Infer = StateT Ctx (ExceptT Error Identity)

{-

The type checker will assume we first rename all variables to unique name, as to not
have to care about scoping. It significantly improves the quality of life of the
programmer.

TODOs:
    Add skolemization variables. i.e
    { \x. 3 : forall a. a -> a }
    should not type check

    Generalize. Not really sure what that means though

-}

run :: Infer a -> Either Error a
run = runIdentity . runExceptT . flip St.evalStateT (Ctx mempty mempty 0)

typecheck :: RProgram -> Either Error TProgram
typecheck = run . inferPrg

inferPrg :: RProgram -> Infer TProgram
inferPrg (RProgram xs) = do
    xs' <- mapM inferBind xs
    return $ TProgram xs'

-- Binds are not correctly added to the context.
-- Can't type check programs with more than one function currently
inferBind :: RBind -> Infer TBind
inferBind b@(RBind name e) = do
    insertSigs name b Nothing
    (t, e') <- inferExp e
    return $ TBind name t e'

-- Have to figure out a way to coerce polymorphic types to monomorphic ones where necessary
-- { \x. \y. x + y } will have the type { a -> b -> Int }
inferExp :: RExp -> Infer (Type, TExp)
inferExp = \case
    RAnn expr typ -> do
        (t, expr') <- inferExp expr
        void $ t =:= typ
        return (typ, expr')
    RBound num name -> do
        t <- lookupVars num
        return (t, TBound num name t)
    RFree name -> do
        (b@(RBind name _), t) <- lookupSigs name
        t' <- case t of
            Nothing -> do
                (TBind _ a _) <- inferBind b
                insertSigs name b (Just a)
                return a
            Just a -> return a
        return (t', TFree name t')
    RConst (CInt i) -> return (TMono "Int", TConst (CInt i) (TMono "Int"))
    RConst (CStr str) -> return (TMono "Str", TConst (CStr str) (TMono "Str"))
    RAdd expr1 expr2 -> do
        (typ1, expr1') <- check expr1 (TMono "Int")
        (_, expr2') <- check expr2 (TMono "Int")
        return (typ1, TAdd expr1' expr2' typ1)
    RApp expr1 expr2 -> do
        (fn_t, expr1') <- inferExp expr1
        (arg_t, expr2') <- inferExp expr2
        res <- fresh
        -- TODO: Double check if this is correct behavior.
        -- It might be the case that we should return res, rather than new_t
        new_t <- fn_t =:= TArrow arg_t res
        return (new_t, TApp expr1' expr2' new_t)
    RAbs num name expr -> do
        arg <- fresh
        insertVars num arg
        (typ, expr') <- inferExp expr
        return (TArrow arg typ, TAbs num name expr' typ)

check :: RExp -> Type -> Infer (Type, TExp)
check e t = do
    (t', e') <- inferExp e
    t'' <- t' =:= t
    return (t'', e')

fresh :: Infer Type
fresh = do
    var <- St.gets nextFresh
    St.modify (\st -> st {nextFresh = succ var})
    return (TPoly $ Ident (show var))

-- | Unify two types.
(=:=) :: Type -> Type -> Infer Type
(=:=) (TPoly _) b = return b
(=:=) a (TPoly _) = return a
(=:=) (TMono a) (TMono b) | a == b = return (TMono a)
(=:=) (TArrow a b) (TArrow c d) = do
    t1 <- a =:= c
    t2 <- b =:= d
    return $ TArrow t1 t2
(=:=) a b = throwError (TypeMismatch $ unwords ["Can not unify type", show a, "with", show b])

-- Unused currently
lookupVars :: Integer -> Infer Type
lookupVars i = do
    st <- St.gets vars
    case M.lookup i st of
        Just t -> return t
        Nothing -> throwError $ UnboundVar "lookupVars"

insertVars :: Integer -> Type -> Infer ()
insertVars i t = do
    st <- St.get
    St.put (st {vars = M.insert i t st.vars})

lookupSigs :: Ident -> Infer (RBind, Maybe Type)
lookupSigs i = do
    st <- St.gets sigs
    case M.lookup i st of
        Just t -> return t
        Nothing -> throwError $ UnboundVar "lookupSigs"

insertSigs :: Ident -> RBind -> Maybe Type -> Infer ()
insertSigs i b t = do
    st <- St.get
    St.put (st {sigs = M.insert i (b, t) st.sigs})

{-# WARNING todo "TODO IN CODE" #-}
todo :: a
todo = error "TODO in code"

data Error
    = TypeMismatch String
    | NotNumber String
    | FunctionTypeMismatch String
    | NotFunction String
    | UnboundVar String
    | AnnotatedMismatch String
    | Default String
    deriving (Show)
