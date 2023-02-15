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

import Debug.Trace (trace)
import TypeChecker.TypeCheckerIr

data Ctx = Ctx { vars :: Map Integer Type
               , sigs :: Map Ident Type
               , count :: Int
               }
    deriving Show

{-

The type checker will assume we first rename all variables to unique name, as to not
have to care about scoping. It significantly improves the quality of life of the
programmer.

-}

type Infer = StateT Ctx (ExceptT Error Identity)

initEnv :: Ctx
initEnv = Ctx mempty mempty 0

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
    t <- inferExp e
    e' <- toTExpr e
    insertSigs name t
    return $ TBind name t e'

-- This needs to be fixed. Should not separate inference of type and creation of the new data type.
toTExpr :: RExp -> Infer TExp
toTExpr = \case

    re@(RAnn e t) -> do
        t <- inferExp re
        e' <- toTExpr e
        return $ TAnn e' t

    re@(RBound num name) -> do
        t <- inferExp re
        return $ TBound num name t

    re@(RFree name) -> do
        t <- inferExp re
        return $ TFree name t

    re@(RConst con)-> do
        t <- inferExp re
        return $ TConst con t

    re@(RApp e1 e2) -> do
        t <- inferExp re
        e1' <- toTExpr e1
        e2' <- toTExpr e2
        return $ TApp e1' e2' t

    re@(RAdd e1 e2)-> do
        t <- inferExp re
        e1' <- toTExpr e1
        e2' <- toTExpr e2
        return $ TAdd e1' e2' t

    re@(RAbs num name e) -> do
        t <- inferExp re
        e' <- toTExpr e
        return $ TAbs num name e' t

inferExp :: RExp -> Infer Type
inferExp = \case

    RAnn expr typ -> do
        exprT <- inferExp expr
        when (not (exprT == typ || isPoly exprT)) (throwError $ AnnotatedMismatch "inferExp, RAnn")
        return typ

    -- Name is only here for proper error messages
    RBound num name ->
        M.lookup num <$> St.gets vars >>= \case
            Nothing -> throwError $ UnboundVar "RBound"
            Just t -> return t

    RFree name -> do
        M.lookup name <$> St.gets sigs >>= \case
            Nothing -> throwError $ UnboundVar "RFree"
            Just t -> return t

    RConst (CInt _) -> return $ TMono "Int"

    RConst (CStr _) -> return $ TMono "Str"

    -- Should do proper unification using union-find. Some nice libs exist
    RApp expr1 expr2 -> do
        typ1 <- inferExp expr1
        typ2 <- inferExp expr2
        cnt <- incCount
        case typ1 of
            (TPoly (Ident x)) -> do 
                let newType = (TArrow (TPoly (Ident x)) (TPoly . Ident $ x ++ (show cnt)))
                specifyType expr1 newType
                apply newType typ1
            _         -> apply typ2 typ1

    RAdd expr1 expr2 -> do
        typ1 <- inferExp expr1
        typ2 <- inferExp expr2
        when (not $ (isInt typ1 || isPoly typ1) && (isInt typ2 || isPoly typ2)) (throwError $ TypeMismatch "inferExp, RAdd")
        specifyType expr1 (TMono "Int")
        specifyType expr2 (TMono "Int")
        return (TMono "Int")

    RAbs num name expr -> do
        insertVars num (TPoly "a")
        typ <- inferExp expr
        newTyp <- lookupVars num
        return $ TArrow newTyp typ

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

incCount :: Infer Int
incCount = do
    st <- St.get
    St.put ( st { count = succ st.count } )
    return st.count

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

union :: Type -> Type -> Infer ()
union = todo

find :: Type -> Type
find = todo

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

bind = RBind "test" fn_on_var
