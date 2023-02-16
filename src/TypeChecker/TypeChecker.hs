{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedRecordDot #-}

module TypeChecker.TypeChecker where

import Control.Monad (when, void)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as St
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map (Map)
import qualified Data.Map as M
import Grammar.ErrM (Err)
import Grammar.Print
import Data.List (findIndex)

import Debug.Trace (trace)
import TypeChecker.TypeCheckerIr

data Ctx = Ctx { vars :: Map Integer Type
               , sigs :: Map Ident Type
               , nextFresh :: Ident
               }
    deriving Show

{-

The type checker will assume we first rename all variables to unique name, as to not
have to care about scoping. It significantly improves the quality of life of the
programmer.

-}

type Infer = StateT Ctx (ExceptT Error Identity)

initEnv :: Ctx
initEnv = Ctx mempty mempty "a"

run :: Infer a -> Either Error a
run = runIdentity . runExceptT . flip St.evalStateT initEnv

typecheck :: RProgram -> Either Error TProgram
typecheck = run . inferPrg 

inferPrg :: RProgram -> Infer TProgram
inferPrg (RProgram xs) = do
    xs' <- mapM inferBind xs
    return $ TProgram xs'

inferBind :: RBind -> Infer TBind
inferBind (RBind name e) = do
    (t, e') <- inferExp e
    insertSigs name t
    return $ TBind name t e'

inferExp :: RExp -> Infer (Type, TExp)
inferExp = \case

    RAnn expr typ -> do
        (t,expr') <- inferExp expr
        when (not (t == typ || isPoly t)) (throwError $ AnnotatedMismatch "inferExp, RAnn")
        return (typ,expr')

    -- Name is only here for proper error messages
    RBound num name ->
        M.lookup num <$> St.gets vars >>= \case
            Nothing -> throwError $ UnboundVar "RBound"
            Just t -> return (t, TBound num name t)

    RFree name -> do
        M.lookup name <$> St.gets sigs >>= \case
            Nothing -> throwError $ UnboundVar "RFree"
            Just t -> return (t, TFree name t)

    RConst (CInt i) -> return $ (TMono "Int", TConst (CInt i) (TMono "Int"))

    RConst (CStr str) -> return $ (TMono "Str", TConst (CStr str) (TMono "Str"))

    -- Should do proper unification using union-find. Some nice libs exist
    RApp expr1 expr2 -> do
        (typ1, expr1') <- inferExp expr1
        (typ2, expr2') <- inferExp expr2
        fvar <- fresh
        case typ1 of
            (TPoly (Ident x)) -> do 
                let newType = (TArrow (TPoly (Ident x)) (TPoly fvar))
                specifyType expr1 newType
                typ1' <- apply newType typ1
                return $ (typ1', TApp expr1' expr2' typ1')
            _         -> (\t -> (t, TApp expr1' expr2' t)) <$> apply typ1 typ2

    RAdd expr1 expr2 -> do
        (typ1, expr1') <- inferExp expr1
        (typ2, expr2') <- inferExp expr2
        when (not $ (isInt typ1 || isPoly typ1) && (isInt typ2 || isPoly typ2)) (throwError $ TypeMismatch "inferExp, RAdd")
        specifyType expr1 (TMono "Int")
        specifyType expr2 (TMono "Int")
        return (TMono "Int", TAdd expr1' expr2' (TMono "Int"))

    RAbs num name expr -> do
        insertVars num (TPoly "a")
        (typ, expr') <- inferExp expr
        newTyp <- lookupVars num
        return $ (TArrow newTyp typ, TAbs num name expr' typ)

-- Aux
isInt :: Type -> Bool
isInt (TMono "Int") = True
isInt _             = False

isArrow :: Type -> Bool
isArrow (TArrow _ _) = True
isArrow _            = False

isPoly :: Type -> Bool
isPoly (TPoly _) = True
isPoly _         = False

fresh :: Infer Ident
fresh = do
    (Ident var) <- St.gets nextFresh
    when (length var == 0) (throwError $ Default "fresh")
    index <- case findIndex (== (head var)) alphabet of
                  Nothing -> throwError $ Default "fresh"
                  Just i -> return i
    let nextIndex = (index + 1) `mod` 26
    let newVar = Ident $ [alphabet !! nextIndex]
    St.modify (\st -> st { nextFresh = newVar })
    return newVar
  where
    alphabet = "abcdefghijklmnopqrstuvwxyz" :: [Char]

unify :: Type -> Type -> Infer Type
unify = todo

-- | Specify the type of a bound variable
-- Because in lambdas we have to assume a general type and update it
specifyType :: RExp -> Type -> Infer ()
specifyType (RBound num name) typ = do
    insertVars num typ
    return ()
specifyType _ _ = return ()

lookupVars :: Integer -> Infer Type
lookupVars i = do
    st <- St.gets vars
    case M.lookup i st of
      Just t -> return t
      Nothing -> throwError $ UnboundVar "lookupVars"

insertVars :: Integer -> Type -> Infer ()
insertVars i t = do
    st <- St.get
    St.put ( st { vars = M.insert i t st.vars } )

lookupSigs :: Ident -> Infer Type
lookupSigs i = do
    st <- St.gets sigs
    case M.lookup i st of
      Just t -> return t
      Nothing -> throwError $ UnboundVar "lookupSigs"

insertSigs :: Ident -> Type -> Infer ()
insertSigs i t = do
    st <- St.get
    St.put ( st { sigs = M.insert i t st.sigs } )

-- Have to figure out the equivalence classes for types.
-- Currently this does not support more than exact matches.
apply :: Type -> Type -> Infer Type
apply (TArrow t1 t2) t3
  | t1 == t3 = return t2
apply t1 t2 = throwError $ TypeMismatch "apply"

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
    deriving Show

-- Tests

lambda = RAbs 0 "x" (RAdd (RBound 0 "x") (RBound 0 "x"))
lambda2 = RAbs 0 "x" (RAnn (RBound 0 "x") (TArrow (TMono "Int") (TMono "String")))

fn_on_var = RAbs 0 (Ident "f") (RAbs 1 (Ident "x") (RApp (RBound 0 (Ident "f")) (RBound 1 (Ident "x"))))


--add x = \y. x+y;
add = RAbs 0 "x" (RAbs 1 "y" (RAdd (RBound 0 "x") (RBound 1 "y")))
-- main = (\z. z+z) ((add 4) 6);
main = RApp (RAbs 0 "z" (RAdd (RBound 0 "z") (RBound 0 "z"))) applyAdd
four = RConst (CInt 4)
six = RConst (CInt 6)
applyAdd = (RApp (RApp add four) six)
partialAdd = RApp add four
