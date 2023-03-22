{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for type checking and inference using algorithm W, Hindley-Milner
module TypeChecker.TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import Data.List (foldl')
import Data.List.Extra (unsnoc)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Grammar.Abs
import Grammar.Print (printTree)
import TypeChecker.TypeCheckerIr (
    Ctx (..),
    Env (..),
    Error,
    Infer,
    Poly (..),
    Subst,
 )
import TypeChecker.TypeCheckerIr qualified as T

initCtx = Ctx mempty

initEnv = Env 0 mempty mempty

runPretty :: Exp -> Either Error String
runPretty = fmap (printTree . fst) . run . inferExp

run :: Infer a -> Either Error a
run = runC initEnv initCtx

runC :: Env -> Ctx -> Infer a -> Either Error a
runC e c = runIdentity . runExceptT . flip runReaderT c . flip evalStateT e

typecheck :: Program -> Either Error T.Program
typecheck = run . checkPrg

checkData :: Data -> Infer ()
checkData d = do
    case d of
        (Data typ@(Indexed name ts) constrs) -> do
            unless
                (all isPoly ts)
                (throwError $ unwords ["Data type incorrectly declared"])
            traverse_
                ( \(Constructor name' t') ->
                    if TIndexed typ == retType t'
                        then insertConstr name' t'
                        else
                            throwError $
                                unwords
                                    [ "return type of constructor:"
                                    , printTree name
                                    , "with type:"
                                    , printTree (retType t')
                                    , "does not match data: "
                                    , printTree typ
                                    ]
                )
                constrs

retType :: Type -> Type
retType (TFun _ t2) = retType t2
retType a = a

checkPrg :: Program -> Infer T.Program
checkPrg (Program bs) = do
    preRun bs
    bs' <- checkDef bs
    return $ T.Program bs'
  where
    preRun :: [Def] -> Infer ()
    preRun [] = return ()
    preRun (x : xs) = case x of
        -- TODO: Check for no overlapping signature definitions
        DSig (Sig n t) -> insertSig n t >> preRun xs
        DBind (Bind{}) -> preRun xs
        DData d@(Data _ _) -> checkData d >> preRun xs

    checkDef :: [Def] -> Infer [T.Def]
    checkDef [] = return []
    checkDef (x : xs) = case x of
        (DBind b) -> do
            b' <- checkBind b
            fmap (T.DBind b' :) (checkDef xs)
        (DData d) -> fmap (T.DData d :) (checkDef xs)
        (DSig _) -> checkDef xs

checkBind :: Bind -> Infer T.Bind
checkBind (Bind name args e) = do
    let lambda = makeLambda e (reverse args)
    e@(_, t') <- inferExp lambda
    -- TODO: Check for match against existing signatures
    return $ T.Bind (coerce name, t') [] e -- (apply s e)
  where
    makeLambda :: Exp -> [LIdent] -> Exp
    makeLambda = foldl (flip EAbs)

{- | Check if two types are considered equal
  For the purpose of the algorithm two polymorphic types are always considered
  equal
-}
typeEq :: Type -> Type -> Bool
typeEq (TFun l r) (TFun l' r') = typeEq l l' && typeEq r r'
typeEq (TLit a) (TLit b) = a == b
typeEq (TIndexed (Indexed name a)) (TIndexed (Indexed name' b)) =
    length a == length b
        && name == name'
        && and (zipWith typeEq a b)
typeEq (TAll n1 t1) (TAll n2 t2) = t1 `typeEq` t2
typeEq _ _ = False

isMoreSpecificOrEq :: T.Type -> T.Type -> Bool
isMoreSpecificOrEq _ (T.TAll _ _) = True
isMoreSpecificOrEq (T.TFun a b) (T.TFun c d) =
    isMoreSpecificOrEq a c && isMoreSpecificOrEq b d
isMoreSpecificOrEq (T.TIndexed (T.Indexed n1 ts1)) (T.TIndexed (T.Indexed n2 ts2)) =
    n1 == n2
        && length ts1 == length ts2
        && and (zipWith isMoreSpecificOrEq ts1 ts2)
isMoreSpecificOrEq a b = a == b

isPoly :: Type -> Bool
isPoly (TAll _ _) = True
isPoly _ = False

inferExp :: Exp -> Infer T.ExpT
inferExp e = do
    (s, t, e') <- algoW e
    let subbed = apply s t
    return $ replace subbed (e', t)

replace :: T.Type -> T.ExpT -> T.ExpT
replace t = second (const t)

class NewType a b where
    toNew :: a -> b

instance NewType Type T.Type where
    toNew = \case
        TLit i -> T.TLit $ coerce i
        TVar v -> T.TVar v
        TFun t1 t2 -> T.TFun (toNew t1) (toNew t2)
        TAll b t -> T.TAll b (toNew t)
        TIndexed i -> T.TIndexed (toNew i)
        TEVar _ -> error "Should not exist after typechecker"

instance NewType Indexed T.Indexed where
    toNew (Indexed name vars) = T.Indexed (coerce name) (map toNew vars)

algoW :: Exp -> Infer (Subst, T.ExpT)
algoW = \case
    -- \| TODO: More testing need to be done. Unsure of the correctness of this
    EAnn e t -> do
        (s1, (e', t')) <- algoW e
        unless
            (toNew t `isMoreSpecificOrEq` t')
            ( throwError $
                unwords
                    [ "Annotated type:"
                    , printTree t
                    , "does not match inferred type:"
                    , printTree t'
                    ]
            )
        applySt s1 $ do
            s2 <- unify (toNew t) t'
            let comp = s2 `compose` s1
            return (comp, (apply comp e', toNew t))

    -- \| ------------------
    -- \|   Γ ⊢ i : Int, ∅

    ELit lit ->
        let lt = toNew $ litType lit
         in return (nullSubst, (T.ELit lt lit, lt))
    -- \| x : σ ∈ Γ   τ = inst(σ)
    -- \| ----------------------
    -- \|     Γ ⊢ x : τ, ∅

    EId i -> do
        var <- asks vars
        case M.lookup i var of
            Just t -> inst (toNew t) >>= \x -> return (nullSubst, x, T.EId (i, x))
            Nothing -> do
                sig <- gets sigs
                case M.lookup i sig of
                    Just t -> return (nullSubst, toNew t, T.EId (i, toNew t))
                    Nothing -> do
                        constr <- gets constructors
                        case M.lookup i constr of
                            Just t -> return (nullSubst, toNew t, T.EId (i, toNew t))
                            Nothing ->
                                throwError $
                                    "Unbound variable: " ++ show i

    -- \| τ = newvar   Γ, x : τ ⊢ e : τ', S
    -- \| ---------------------------------
    -- \|     Γ ⊢ w λx. e : Sτ → τ', S

    EAbs name e -> do
        fr <- fresh
        withBinding (coerce name) (Forall [] (toNew fr)) $ do
            (s1, t', e') <- algoW e
            let varType = toNew $ apply s1 fr
            let newArr = T.TFun varType (toNew t')
            return (s1, newArr, apply s1 $ T.EAbs newArr (coerce name, varType) (e', newArr))

    -- \| Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S₁
    -- \| s₂ = mgu(s₁τ₀, Int)    s₃ = mgu(s₂τ₁, Int)
    -- \| ------------------------------------------
    -- \|        Γ ⊢ e₀ + e₁ : Int, S₃S₂S₁S₀
    -- This might be wrong

    EAdd e0 e1 -> do
        (s1, t0, e0') <- algoW e0
        applySt s1 $ do
            (s2, t1, e1') <- algoW e1
            -- applySt s2 $ do
            s3 <- unify (apply s2 t0) (T.TLit "Int")
            s4 <- unify (apply s3 t1) (T.TLit "Int")
            let comp = s4 `compose` s3 `compose` s2 `compose` s1
            return
                ( comp
                , T.TLit "Int"
                , apply comp $ T.EAdd (T.TLit "Int") (e0', t0) (e1', t1)
                )

    -- \| Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S1
    -- \| τ' = newvar    S₂ = mgu(S₁τ₀, τ₁ → τ')
    -- \| --------------------------------------
    -- \|       Γ ⊢ e₀ e₁ : S₂τ', S₂S₁S₀

    EApp e0 e1 -> do
        fr <- toNew <$> fresh
        (s0, t0, e0') <- algoW e0
        applySt s0 $ do
            (s1, t1, e1') <- algoW e1
            -- applySt s1 $ do
            s2 <- unify (apply s1 t0) (T.TFun (toNew t1) fr)
            let t = apply s2 fr
            let comp = s2 `compose` s1 `compose` s0
            return (comp, t, apply comp $ T.EApp t (e0', t0) (e1', t1))

    -- \| Γ ⊢ e₀ : τ, S₀     S₀Γ, x : S̅₀Γ̅(τ) ⊢ e₁ : τ', S₁
    -- \| ----------------------------------------------
    -- \|        Γ ⊢ let x = e₀ in e₁ : τ', S₁S₀

    -- The bar over S₀ and Γ means "generalize"

    ELet name e0 e1 -> do
        (s1, t1, e0') <- algoW e0
        env <- asks vars
        let t' = generalize (apply s1 env) t1
        withBinding name t' $ do
            (s2, t2, e1') <- algoW e1
            let comp = s2 `compose` s1
            return (comp, t2, apply comp $ T.ELet (T.Bind (name, t2) e0') e1')
    ECase caseExpr injs -> do
        (sub, t, e') <- algoW caseExpr
        (subst, injs, ret_t) <- checkCase t injs
        let comp = subst `compose` sub
        let t' = apply comp ret_t
        return (comp, t', T.ECase t' e' injs)

-- | Unify two types producing a new substitution
unify :: T.Type -> T.Type -> Infer Subst
unify t0 t1 = do
    case (t0, t1) of
        (T.TFun a b, T.TFun c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s1 `compose` s2
        (T.TVar t, b) -> occurs b t
        (a, T.TVar t) -> occurs a t
        (T.TAll _ t, b) -> unify t b
        (a, T.TAll _ t) -> unify a t
        (T.TLit a, T.TLit b) ->
            if a == b then return M.empty else throwError "Types do not unify"
        (T.TIndexed (T.Indexed name t), T.TIndexed (T.Indexed name' t')) ->
            if name == name' && length t == length t'
                then do
                    xs <- zipWithM unify t t'
                    return $ foldr compose nullSubst xs
                else
                    throwError $
                        unwords
                            [ "T.Type constructor:"
                            , printT . Tree name
                            , "(" ++ printT . Tree t ++ ")"
                            , "does not match with:"
                            , printT . Tree name'
                            , "(" ++ printT . Tree t' ++ ")"
                            ]
        (a, b) -> do
            ctx <- ask
            env <- get
            throwError . unwords $
                [ "T.Type:"
                , printT . Tree a
                , "can't be unified with:"
                , printT . Tree b
                , "\nCtx:"
                , show ctx
                , "\nEnv:"
                , show env
                ]

{- | Check if a type is contained in another type.
I.E. { a = a -> b } is an unsolvable constraint since there is no substitution
where these are equal
-}
occurs :: LIdent -> T.Type -> Infer Subst
occurs i t@(T.TVar _) = return (M.singleton i t)
occurs i t =
    if S.member i (free t)
        then
            throwError $
                unwords
                    [ "Occurs check failed, can't unify"
                    , printTree (TVar $ MkTVar i)
                    , "with"
                    , printTree t
                    ]
        else return $ M.singleton i t

-- | Generalize a type over all free variables in the substitution set
generalize :: Map Ident Poly -> Type -> Poly
generalize env t = Forall (S.toList $ free t S.\\ free env) t

{- | Instantiate a polymorphic type. The free type variables are substituted
with fresh ones.
-}
inst :: T.Type -> Infer T.Type
inst = \case
    T.TAll bound t -> do
        fr <- fresh
        let s = M.singleton fr bound
        apply s <$> inst t
    _ -> undefined

-- | Compose two substitution sets
compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (apply m1) m2 `M.union` m1

-- TODO: Split this class into two separate classes, one for free variables
--       and one for applying substitutions

-- | A class representing free variables functions
class FreeVars t where
    -- | Get all free variables from t
    free :: t -> Set LIdent

    -- | Apply a substitution to t
    apply :: Subst -> t -> t

instance FreeVars T.Type where
    free :: T.Type -> Set LIdent
    free (T.TVar (MkTVar a)) = S.singleton a
    free (T.TAll (MkTVar bound) t) = (S.singleton bound) `S.intersection` free t
    free (T.TLit _) = mempty
    free (T.TFun a b) = free a `S.union` free b
    -- \| Not guaranteed to be correct
    free (T.TIndexed (T.Indexed _ a)) =
        foldl' (\acc x -> free x `S.union` acc) S.empty a

    apply :: Subst -> T.Type -> T.Type
    apply sub t = do
        case t of
            T.TLit a -> T.TLit a
            T.TVar (MkTVar a) -> case M.lookup a sub of
                Nothing -> T.TVar (MkTVar a)
                Just t -> t
            T.TAll bound t -> undefined
            T.TFun a b -> T.TFun (apply sub a) (apply sub b)
            T.TIndexed (T.Indexed name a) -> T.TIndexed (T.Indexed name (map (apply sub) a))

instance FreeVars Poly where
    free :: Poly -> Set LIdent
    free (Forall xs t) = free t S.\\ S.fromList xs
    apply :: Subst -> Poly -> Poly
    apply s (Forall xs t) = Forall xs (apply (foldr M.delete s xs) t)

instance FreeVars (Map LIdent Poly) where
    free :: Map LIdent Poly -> Set LIdent
    free m = foldl' S.union S.empty (map free $ M.elems m)
    apply :: Subst -> Map LIdent Poly -> Map LIdent Poly
    apply s = M.map (apply s)

instance FreeVars T.Exp where
    free :: T.Exp -> Set LIdent
    free = error "free not implemented for T.Exp"
    apply :: Subst -> T.Exp -> T.Exp
    apply s = \case
        T.EId (ident, t) ->
            T.EId (ident, apply s t)
        T.ELit t lit ->
            T.ELit (apply s t) lit
        T.ELet (T.Bind (ident, t) args e1) e2 ->
            T.ELet (T.Bind (ident, apply s t) args (apply s e1)) (apply s e2)
        T.EApp t e1 e2 ->
            T.EApp (apply s t) (apply s e1) (apply s e2)
        T.EAdd t e1 e2 ->
            T.EAdd (apply s t) (apply s e1) (apply s e2)
        T.EAbs t1 (ident, t2) e ->
            T.EAbs (apply s t1) (ident, apply s t2) (apply s e)
        T.ECase t e injs ->
            T.ECase (apply s t) (apply s e) (apply s injs)

instance FreeVars T.Inj where
    free :: T.Inj -> Set LIdent
    free = undefined
    apply :: Subst -> T.Inj -> T.Inj
    apply s (T.Inj (i, t) e) = T.Inj (i, apply s t) (apply s e)

instance FreeVars [T.Inj] where
    free :: [T.Inj] -> Set LIdent
    free = foldl' (\acc x -> free x `S.union` acc) mempty
    apply s = map (apply s)

-- | Apply substitutions to the environment.
applySt :: Subst -> Infer a -> Infer a
applySt s = local (\st -> st{vars = apply s (vars st)})

-- | Represents the empty substition set
nullSubst :: Subst
nullSubst = M.empty

-- | Generate a new fresh variable and increment the state counter
fresh :: Infer Type
fresh = do
    n <- gets count
    modify (\st -> st{count = n + 1})
    return . TVar . MkTVar . LIdent $ show n

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => Ident -> Poly -> m a -> m a
withBinding i p = local (\st -> st{vars = M.insert i p (vars st)})

-- | Run the monadic action with several additional bindings
withBindings :: (Monad m, MonadReader Ctx m) => [(Ident, Poly)] -> m a -> m a
withBindings xs =
    local (\st -> st{vars = foldl' (flip (uncurry M.insert)) (vars st) xs})

-- | Insert a function signature into the environment
insertSig :: LIdent -> Type -> Infer ()
insertSig i t = modify (\st -> st{sigs = M.insert i t (sigs st)})

-- | Insert a constructor with its data type
insertConstr :: UIdent -> Type -> Infer ()
insertConstr i t =
    modify (\st -> st{constructors = M.insert i t (constructors st)})

-------- PATTERN MATCHING ---------

checkCase :: Type -> [Inj] -> Infer (Subst, [T.Inj], Type)
checkCase expT injs = do
    (injTs, injs, returns) <- unzip3 <$> mapM checkInj injs
    (sub1, _) <-
        foldM
            ( \(sub, acc) x ->
                (\a -> (a `compose` sub, a `apply` acc)) <$> unify x acc
            )
            (nullSubst, expT)
            injTs
    (sub2, returns_type) <-
        foldM
            ( \(sub, acc) x ->
                (\a -> (a `compose` sub, a `apply` acc)) <$> unify x acc
            )
            (nullSubst, head returns)
            (tail returns)
    return (sub2 `compose` sub1, injs, returns_type)

{- | fst = type of init
   | snd = type of expr
-}
checkInj :: Inj -> Infer (Type, T.Inj, Type)
checkInj (Inj it expr) = do
    (initT, vars) <- inferInit it
    let converted = map (second (Forall [])) vars
    (exprT, e) <- withBindings converted (inferExp expr)
    return (initT, T.Inj (it, initT) e, exprT)

inferInit :: Init -> Infer (Type, [T.Id])
inferInit = \case
    InitLit lit -> return (litType lit, mempty)
    InitConstructor fn vars -> do
        gets (M.lookup fn . constructors) >>= \case
            Nothing ->
                throwError $
                    "Constructor: " ++ printTree fn ++ " does not exist"
            Just a -> do
                case unsnoc $ flattenType a of
                    Nothing -> throwError "Partial pattern match not allowed"
                    Just (vs, ret) ->
                        case length vars `compare` length vs of
                            EQ -> do
                                return (ret, zip vars vs)
                            _ -> throwError "Partial pattern match not allowed"
    InitCatch -> (,mempty) <$> fresh

flattenType :: Type -> [Type]
flattenType (TFun a b) = flattenType a ++ flattenType b
flattenType a = [a]

litType :: Lit -> Type
litType (LInt _) = TLit "Int"
litType (LChar _) = TLit "Char"
