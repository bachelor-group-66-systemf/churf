-- {-# LANGUAGE LambdaCase          #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedStrings   #-}

module TypeChecker.TypeChecker where

-- import           Control.Monad             (void)
-- import           Control.Monad.Except      (ExceptT, runExceptT, throwError)
-- import           Control.Monad.State       (StateT)
-- import qualified Control.Monad.State       as St
-- import           Data.Functor.Identity     (Identity, runIdentity)
-- import           Data.Map                  (Map)
-- import qualified Data.Map                  as M

-- import           TypeChecker.TypeCheckerIr

-- data Ctx = Ctx
--     { vars      :: Map Integer Type
--     , sigs      :: Map Ident Type
--     , nextFresh :: Int
--     }
--     deriving (Show)

-- -- Perhaps swap over to reader monad instead for vars and sigs.
-- type Infer = StateT Ctx (ExceptT Error Identity)

-- {-

-- The type checker will assume we first rename all variables to unique name, as to not
-- have to care about scoping. It significantly improves the quality of life of the
-- programmer.

-- TODOs:
--     Add skolemization variables. i.e
--     { \x. 3 : forall a. a -> a }
--     should not type check

--     Generalize. Not really sure what that means though

-- -}

-- typecheck :: RProgram -> Either Error TProgram
-- typecheck = todo

-- run :: Infer a -> Either Error a
-- run = runIdentity . runExceptT . flip St.evalStateT (Ctx mempty mempty 0)

-- -- Have to figure out a way to coerce polymorphic types to monomorphic ones where necessary
-- -- { \x. \y. x + y } will have the type { a -> b -> Int }
-- inferExp :: RExp -> Infer Type
-- inferExp = \case

--     RAnn expr typ -> do
--         t <- inferExp expr
--         void $ t =:= typ
--         return t

--     RBound num name -> lookupVars num

--     RFree name -> lookupSigs name

--     RConst (CInt i) -> return $ TMono "Int"

--     RConst (CStr str) -> return $ TMono "Str"

--     RAdd expr1 expr2 -> do
--         let int = TMono "Int"
--         typ1 <- check expr1 int
--         typ2 <- check expr2 int
--         return int

--     RApp expr1 expr2 -> do
--         fn_t <- inferExp expr1
--         arg_t <- inferExp expr2
--         res <- fresh
--         new_t <- fn_t =:= TArrow arg_t res
--         return res

--     RAbs num name expr -> do
--         arg <- fresh
--         insertVars num arg
--         typ <- inferExp expr
--         return $ TArrow arg typ

-- check :: RExp -> Type -> Infer ()
-- check e t = do
--     t' <- inferExp e
--     t =:= t'
--     return ()

-- fresh :: Infer Type
-- fresh = do
--     var <- St.gets nextFresh
--     St.modify (\st -> st {nextFresh = succ var})
--     return (TPoly $ Ident (show var))

-- -- | Unify two types.
-- (=:=) :: Type -> Type -> Infer Type
-- (=:=) (TPoly _) b = return b
-- (=:=) a (TPoly _) = return a
-- (=:=) (TMono a) (TMono b) | a == b = return (TMono a)
-- (=:=) (TArrow a b) (TArrow c d) = do
--     t1 <- a =:= c
--     t2 <- b =:= d
--     return $ TArrow t1 t2
-- (=:=) a b = throwError (TypeMismatch $ unwords ["Can not unify type", show a, "with", show b])

-- lookupVars :: Integer -> Infer Type
-- lookupVars i = do
--     st <- St.gets vars
--     case M.lookup i st of
--        Just t  -> return t
--        Nothing -> throwError $ UnboundVar "lookupVars"

-- insertVars :: Integer -> Type -> Infer ()
-- insertVars i t = do
--     st <- St.get
--     St.put (st {vars = M.insert i t st.vars})

-- lookupSigs :: Ident -> Infer Type
-- lookupSigs i = do
--     st <- St.gets sigs
--     case M.lookup i st of
--         Just t  -> return t
--         Nothing -> throwError $ UnboundVar "lookupSigs"

-- insertSigs :: Ident -> Type -> Infer ()
-- insertSigs i t = do
--     st <- St.get
--     St.put (st {sigs = M.insert i t st.sigs})

-- {-# WARNING todo "TODO IN CODE" #-}
-- todo :: a
-- todo = error "TODO in code"

-- data Error
--     = TypeMismatch String
--     | NotNumber String
--     | FunctionTypeMismatch String
--     | NotFunction String
--     | UnboundVar String
--     | AnnotatedMismatch String
--     | Default String
--     deriving (Show)


-- {-

-- The procedure inst(σ) specializes the polytype
-- σ by copying the term and replacing the bound type variables
-- consistently by new monotype variables.

-- -}
