{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use traverse_" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use zipWithM" #-}

module TypeChecker.TypeChecker where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity     (Identity, runIdentity)
import           Data.List                 (foldl')
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S

import           Data.Foldable             (traverse_)
import           Grammar.Abs
import           Grammar.Print             (printTree)
import qualified TypeChecker.TypeCheckerIr as T

-- | A data type representing type variables
data Poly = Forall [Ident] Type
    deriving Show

newtype Ctx = Ctx { vars   :: Map Ident Poly
               }

data Env = Env { count  :: Int
               , sigs   :: Map Ident Type
               , dtypes :: Map Ident Type
               }

type Error = String
type Subst = Map Ident Type

type Infer = StateT Env (ReaderT Ctx (ExceptT Error Identity))

initCtx = Ctx mempty
initEnv = Env 0 mempty mempty

runPretty :: Exp -> Either Error String
runPretty = fmap (printTree . fst). run . inferExp

run :: Infer a -> Either Error a
run = runC initEnv initCtx

runC :: Env -> Ctx -> Infer a -> Either Error a
runC e c = runIdentity . runExceptT . flip runReaderT c . flip evalStateT e

typecheck :: Program -> Either Error T.Program
typecheck = run . checkPrg

checkData :: Data -> Infer ()
checkData d = case d of
    (Data typ@(TConstr name _) constrs) -> do
        traverse_ (\(Constructor name' t')
                    -> if typ == retType t'
                          then insertConstr name' t' else
                          throwError $
                              unwords
                              [ "return type of constructor:"
                              , printTree name
                              , "with type:"
                              , printTree (retType t')
                              , "does not match data: "
                              , printTree typ]) constrs
    _ -> throwError "Data type incorrectly declared"
  where
    retType :: Type -> Type
    retType (TArr _ t2) = retType t2
    retType a           = a

checkPrg :: Program -> Infer T.Program
checkPrg (Program bs) = do
    preRun bs
    T.Program <$> checkDef bs
  where
    preRun :: [Def] -> Infer ()
    preRun [] = return ()
    preRun (x:xs) = case x of
        DBind (Bind n t _ _ _ ) -> insertSig n t >> preRun xs
        DData d@(Data _ _)      -> checkData d >> preRun xs

    checkDef :: [Def] -> Infer [T.Def]
    checkDef [] = return []
    checkDef (x:xs) = case x of
        (DBind b) -> do
            b' <- checkBind b
            fmap (T.DBind b' :) (checkDef xs)
        (DData d)    -> fmap (T.DData d :) (checkDef xs)

checkBind :: Bind -> Infer T.Bind
checkBind (Bind n t _ args e) = do
    (t', e') <- inferExp $ makeLambda e (reverse args)
    s <- unify t t'
    let t'' = apply s t
    unless (t `typeEq` t'') (throwError $ unwords ["Top level signature", printTree t, "does not match body with type:", printTree t''])
    return $ T.Bind (n, t) [] e'
  where
    makeLambda :: Exp -> [Ident] -> Exp
    makeLambda = foldl (flip EAbs)

-- | Check if two types are considered equal
--   For the purpose of the algorithm two polymorphic types are always considered equal
typeEq :: Type -> Type -> Bool
typeEq (TArr l r) (TArr l' r')            = typeEq l l' && typeEq r r'
typeEq (TMono a) (TMono b)                = a == b
typeEq (TConstr name a) (TConstr name' b) = name == name' && and (zipWith typeEq a b)
typeEq (TPol _) (TPol _)                  = True
typeEq _ _                                = False

inferExp :: Exp -> Infer (Type, T.Exp)
inferExp e = do
    (s, t, e') <- algoW e
    let subbed = apply s t
    return (subbed, replace subbed e')

replace :: Type -> T.Exp -> T.Exp
replace t = \case
    T.ELit _ e -> T.ELit t e
    T.EId (n, _) -> T.EId (n, t)
    T.EAbs _ name e -> T.EAbs t name e
    T.EApp _ e1 e2 -> T.EApp t e1 e2
    T.EAdd _ e1 e2 -> T.EAdd t e1 e2
    T.ELet (T.Bind (n, _) args e1) e2 -> T.ELet (T.Bind (n, t) args e1) e2

algoW :: Exp -> Infer (Subst, Type, T.Exp)
algoW = \case

    EAnn e t -> do
        (s1, t', e') <- algoW e
        applySt s1 $ do
            s2 <- unify (apply s1 t) t'
            return (s2 `compose` s1, t, e')

-- | ------------------
-- |   Γ ⊢ e₀ : Int, ∅

    ELit (LInt n) -> return (nullSubst, TMono "Int", T.ELit  (TMono "Int") (LInt n))

    ELit a -> error $ "NOT IMPLEMENTED YET: ELit " ++ show a

-- | x : σ ∈ Γ   τ = inst(σ)
-- | ----------------------
-- |     Γ ⊢ x : τ, ∅

    EId i -> do
        var <- asks vars
        case M.lookup i var of
          Just t  -> inst t >>= \x -> return (nullSubst, x, T.EId (i, x))
          Nothing -> do
              sig <- gets sigs
              case M.lookup i sig of
                  Just t  -> return (nullSubst, t, T.EId (i, t))
                  Nothing -> do
                    constr <- gets dtypes
                    case M.lookup i constr of
                      Just t  -> return (nullSubst, t, T.EId (i, t))
                      Nothing -> throwError $ "Unbound variable: " ++ show i

-- | τ = newvar   Γ, x : τ ⊢ e : τ', S
-- | ---------------------------------
-- |     Γ ⊢ w λx. e : Sτ → τ', S

    EAbs name e -> do
        fr <- fresh
        withBinding name (Forall [] fr) $ do
            (s1, t', e') <- algoW e
            let varType = apply s1 fr
            let newArr = TArr varType t'
            return (s1, newArr, T.EAbs newArr (name, varType) e')

-- | Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S₁
-- | s₂ = mgu(s₁τ₀, Int)    s₃ = mgu(s₂τ₁, Int)
-- | ------------------------------------------
-- |        Γ ⊢ e₀ + e₁ : Int, S₃S₂S₁S₀
-- This might be wrong

    EAdd e0 e1 -> do
        (s1, t0, e0') <- algoW e0
        applySt s1 $ do
            (s2, t1, e1') <- algoW e1
            -- applySt s2 $ do
            s3 <- unify (apply s2 t0) (TMono "Int")
            s4 <- unify (apply s3 t1) (TMono "Int")
            return (s4 `compose` s3 `compose` s2 `compose` s1, TMono "Int", T.EAdd (TMono "Int") e0' e1')

-- | Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S1
-- | τ' = newvar    S₂ = mgu(S₁τ₀, τ₁ → τ')
-- | --------------------------------------
-- |       Γ ⊢ e₀ e₁ : S₂τ', S₂S₁S₀

    EApp e0 e1 -> do
        fr <- fresh
        (s0, t0, e0') <- algoW e0
        applySt s0 $ do
            (s1, t1, e1') <- algoW e1
            -- applySt s1 $ do
            s2 <- unify (apply s1 t0) (TArr t1 fr)
            let t = apply s2 fr
            return (s2 `compose` s1 `compose` s0, t, T.EApp t e0' e1')

-- | Γ ⊢ e₀ : τ, S₀     S₀Γ, x : S̅₀Γ̅(τ) ⊢ e₁ : τ', S₁
-- | ----------------------------------------------
-- |        Γ ⊢ let x = e₀ in e₁ : τ', S₁S₀

-- The bar over S₀ and Γ means "generalize"

    ELet name e0 e1 -> do
        (s1, t1, e0') <- algoW e0
        env <- asks vars
        let t' = generalize (apply s1 env) t1
        withBinding name t' $ do
            (s2, t2, e1') <- algoW e1
            return (s2 `compose` s1, t2, T.ELet (T.Bind (name,t2) [] e0') e1' )

    ECase a b -> error $ "NOT IMPLEMENTED YET: ECase" ++ show a ++ " " ++ show b

-- | Unify two types producing a new substitution (constraint)
unify :: Type -> Type -> Infer Subst
unify t0 t1 = case (t0, t1) of
    (TArr a b, TArr c d) -> do
        s1 <- unify a c
        s2 <- unify (apply s1 b) (apply s1 d)
        return $ s1 `compose` s2
    (TPol a, b) -> occurs a b
    (a, TPol b) -> occurs b a
    (TMono a, TMono b) -> if a == b then return M.empty else throwError "Types do not unify"
    -- | TODO: Figure out a cleaner way to express the same thing
    (TConstr name t, TConstr name' t') -> if name == name' && length t == length t'
                                            then do
                                                xs <- sequence $ zipWith unify t t'
                                                return $ foldr compose nullSubst xs
                                            else throwError $ unwords ["Type constructor:", printTree name, "(" ++ printTree t ++ ")", "does not match with:", printTree name', "(" ++ printTree t' ++ ")"]
    (a, b) -> throwError . unwords $ ["Type:", printTree a, "can't be unified with:", printTree b]

-- | Check if a type is contained in another type.
-- I.E. { a = a -> b } is an unsolvable constraint since there is no substitution such that these are equal
occurs :: Ident -> Type -> Infer Subst
occurs _ (TPol _) = return nullSubst
occurs i t = if S.member i (free t)
                then throwError "Occurs check failed"
                else return $ M.singleton i t

-- | Generalize a type over all free variables in the substitution set
generalize :: Map Ident Poly -> Type -> Poly
generalize env t = Forall (S.toList $ free t S.\\ free env) t

-- | Instantiate a polymorphic type. The free type variables are substituted with fresh ones.
inst :: Poly -> Infer Type
inst (Forall xs t) = do
    xs' <- mapM (const fresh) xs
    let s = M.fromList $ zip xs xs'
    return $ apply s t

-- | Compose two substitution sets
compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (apply m1) m2 `M.union` m1

-- | A class representing free variables functions
class FreeVars t where
  -- | Get all free variables from t
  free :: t -> Set Ident
  -- | Apply a substitution to t
  apply :: Subst -> t -> t

instance FreeVars Type where
  free :: Type -> Set Ident
  free (TPol a)      = S.singleton a
  free (TMono _)     = mempty
  free (TArr a b)    = free a `S.union` free b
  -- | Not guaranteed to be correct
  free (TConstr _ a) = foldl' (\acc x -> free x `S.union` acc) S.empty a
  apply :: Subst -> Type -> Type
  apply sub t = do
    case t of
        TMono a -> TMono a
        TPol a -> case M.lookup a sub of
                   Nothing -> TPol a
                   Just t  -> t
        TArr a b -> TArr (apply sub a) (apply sub b)
        TConstr name a -> TConstr name (map (apply sub) a)

instance FreeVars Poly where
  free :: Poly -> Set Ident
  free (Forall xs t) = free t S.\\ S.fromList xs
  apply :: Subst -> Poly -> Poly
  apply s (Forall xs t) = Forall xs (apply (foldr M.delete s xs) t)

instance FreeVars (Map Ident Poly) where
  free :: Map Ident Poly -> Set Ident
  free m = foldl' S.union S.empty (map free $ M.elems m)
  apply :: Subst -> Map Ident Poly -> Map Ident Poly
  apply s = M.map (apply s)

-- | Apply substitutions to the environment.
applySt :: Subst -> Infer a -> Infer a
applySt s = local (\st -> st { vars = apply s (vars st) })

-- | Represents the empty substition set
nullSubst :: Subst
nullSubst = M.empty

-- | Generate a new fresh variable and increment the state counter
fresh :: Infer Type
fresh = do
    n <- gets count
    modify (\st -> st { count = n + 1 })
    return . TPol . Ident $ "t" ++ show n

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => Ident -> Poly -> m a -> m a
withBinding i p = local (\st -> st { vars = M.insert i p (vars st) })

-- | Insert a function signature into the environment
insertSig :: Ident -> Type -> Infer ()
insertSig i t = modify (\st -> st { sigs = M.insert i t (sigs st) })

-- | Insert a constructor with its data type
insertConstr :: Ident -> Type -> Infer ()
insertConstr i t = modify (\st -> st { dtypes = M.insert i t (dtypes st) })
