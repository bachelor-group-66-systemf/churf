{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for type checking and inference using algorithm W, Hindley-Milner
module TypeChecker.TypeChecker where

import Auxiliary
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
        (Data typ@(TData name ts) constrs) -> do
            unless
                (all isPoly ts)
                (throwError $ unwords ["Data type incorrectly declared"])
            traverse_
                ( \(Constructor name' t') ->
                    if typ == retType t'
                        then insertConstr (coerce name') (toNew t')
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
        _ ->
            throwError $
                "incorrectly declared data type '"
                    <> printTree d
                    <> "'"

retType :: Type -> Type
retType (TFun _ t2) = retType t2
retType a = a

checkPrg :: Program -> Infer T.Program
checkPrg (Program bs) = do
    preRun bs
    -- Type check the program twice to produce all top-level types in the first pass through
    bs' <- checkDef bs
    trace ("FIRST ITERATION: " <> printTree bs') pure ()
    bs'' <- checkDef bs
    return $ T.Program bs''
  where
    preRun :: [Def] -> Infer ()
    preRun [] = return ()
    preRun (x : xs) = case x of
        DSig (Sig n t) -> do
            gets (M.member (coerce n) . sigs)
                >>= flip
                    when
                    ( throwError $
                        "Duplicate signatures for function '"
                            <> printTree n
                            <> "'"
                    )
            insertSig (coerce n) (Just $ toNew t) >> preRun xs
        DBind (Bind n _ _) -> do
            s <- gets sigs
            case M.lookup (coerce n) s of
                Nothing -> insertSig (coerce n) Nothing >> preRun xs
                Just _ -> preRun xs
        DData d@(Data _ _) -> checkData d >> preRun xs

    checkDef :: [Def] -> Infer [T.Def]
    checkDef [] = return []
    checkDef (x : xs) = case x of
        (DBind b) -> do
            b' <- checkBind b
            fmap (T.DBind b' :) (checkDef xs)
        (DData d) -> fmap (T.DData (toNew d) :) (checkDef xs)
        (DSig _) -> checkDef xs

checkBind :: Bind -> Infer T.Bind
checkBind (Bind name args e) = do
    let lambda = makeLambda e (reverse (coerce args))
    (_, lambdaT) <- inferExp lambda
    args <- zip args <$> mapM (const fresh) args
    withBindings (map coerce args) $ do
        e@(_, _) <- inferExp e
        s <- gets sigs
        -- let fs = map (second Just) (getFunctionTypes s e)
        -- mapM_ (uncurry insertSig) fs
        case M.lookup (coerce name) s of
            Just (Just t) -> do
                sub <- unify t lambdaT
                let newT = apply sub t
                insertSig (coerce name) (Just newT)
                return $ T.Bind (coerce name, newT) (map coerce args) e
            _ -> do
                insertSig (coerce name) (Just lambdaT)
                return (T.Bind (coerce name, lambdaT) (map coerce args) e) -- (apply s e)
                -- where
                --   getFunctionTypes :: Map T.Ident (Maybe T.Type) -> T.ExpT -> [(T.Ident, T.Type)]
                --   getFunctionTypes s = \case
                --       (T.EId b, t) -> case M.lookup b s of
                --           Just Nothing -> return (b, t)
                --           _ -> []
                --       (T.ELit _, _) -> []
                --       (T.ELet (T.Bind _ _ e1) e2, _) -> getFunctionTypes s e1 <> getFunctionTypes s e2
                --       (T.EApp e1 e2, _) -> getFunctionTypes s e1 <> getFunctionTypes s e2
                --       (T.EAdd e1 e2, _) -> getFunctionTypes s e1 <> getFunctionTypes s e2
                --       (T.EAbs _ e, _) -> getFunctionTypes s e
                --       (T.ECase e injs, _) -> getFunctionTypes s e <> concatMap (getFunctionTypes s . \(T.Inj _ e) -> e) injs

isMoreSpecificOrEq :: T.Type -> T.Type -> Bool
isMoreSpecificOrEq _ (T.TAll _ _) = True
isMoreSpecificOrEq (T.TFun a b) (T.TFun c d) =
    isMoreSpecificOrEq a c && isMoreSpecificOrEq b d
isMoreSpecificOrEq (T.TData n1 ts1) (T.TData n2 ts2) =
    n1 == n2
        && length ts1 == length ts2
        && and (zipWith isMoreSpecificOrEq ts1 ts2)
isMoreSpecificOrEq a b = a == b

isPoly :: Type -> Bool
isPoly (TAll _ _) = True
isPoly (TVar _) = True
isPoly _ = False

inferExp :: Exp -> Infer T.ExpT
inferExp e = do
    (s, (e', t)) <- algoW e
    let subbed = apply s t
    return $ replace subbed (e', t)

replace :: T.Type -> T.ExpT -> T.ExpT
replace t = second (const t)

class NewType a b where
    toNew :: a -> b

instance NewType Type T.Type where
    toNew = \case
        TLit i -> T.TLit $ coerce i
        TVar v -> T.TVar $ toNew v
        TFun t1 t2 -> T.TFun (toNew t1) (toNew t2)
        TAll b t -> T.TAll (toNew b) (toNew t)
        TData i ts -> T.TData (coerce i) (map toNew ts)
        TEVar _ -> error "Should not exist after typechecker"

instance NewType Lit T.Lit where
    toNew (LInt i) = T.LInt i
    toNew (LChar i) = T.LChar i

instance NewType Data T.Data where
    toNew (Data t xs) = T.Data (name $ retType t) (toNew xs)
      where
        name (TData n _) = coerce n
        name _ = error "Bug in toNew Data -> T.Data"

instance NewType Constructor T.Constructor where
    toNew (Constructor name xs) = T.Constructor (coerce name) (toNew xs)

instance NewType TVar T.TVar where
    toNew (MkTVar i) = T.MkTVar $ coerce i

instance NewType a b => NewType [a] [b] where
    toNew = map toNew

algoW :: Exp -> Infer (Subst, T.ExpT)
algoW = \case
    -- \| TODO: More testing need to be done. Unsure of the correctness of this
    err@(EAnn e t) -> do
        (s1, (e', t')) <- exprErr (algoW e) err
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
            s2 <- exprErr (unify (toNew t) t') err
            let comp = s2 `compose` s1
            return (comp, apply comp (e', toNew t))

    -- \| ------------------
    -- \|   Γ ⊢ i : Int, ∅

    ELit lit -> return (nullSubst, (T.ELit $ toNew lit, litType lit))
    -- \| x : σ ∈ Γ   τ = inst(σ)
    -- \| ----------------------
    -- \|     Γ ⊢ x : τ, ∅
    EVar i -> do
        var <- asks vars
        case M.lookup (coerce i) var of
            Just t -> inst t >>= \x -> return (nullSubst, (T.EId $ coerce i, x))
            Nothing -> do
                sig <- gets sigs
                case M.lookup (coerce i) sig of
                    Just (Just t) -> return (nullSubst, (T.EId $ coerce i, t))
                    Just Nothing ->
                        (\x -> (nullSubst, (T.EId $ coerce i, x))) <$> fresh
                    Nothing -> throwError $ "Unbound variable: " <> printTree i
    EInj i -> do
        constr <- gets constructors
        case M.lookup (coerce i) constr of
            Just t -> return (nullSubst, (T.EId $ coerce i, t))
            Nothing ->
                throwError $
                    "Constructor: '"
                        <> printTree i
                        <> "' is not defined"

    -- \| τ = newvar   Γ, x : τ ⊢ e : τ', S
    -- \| ---------------------------------
    -- \|     Γ ⊢ w λx. e : Sτ → τ', S

    err@(EAbs name e) -> do
        fr <- fresh
        exprErr
            ( withBinding (coerce name) fr $ do
                (s1, (e', t')) <- exprErr (algoW e) err
                let varType = apply s1 fr
                let newArr = T.TFun varType t'
                return (s1, apply s1 (T.EAbs (coerce name) (e', t'), newArr))
            )
            err

    -- \| Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S₁
    -- \| s₂ = mgu(s₁τ₀, Int)    s₃ = mgu(s₂τ₁, Int)
    -- \| ------------------------------------------
    -- \|        Γ ⊢ e₀ + e₁ : Int, S₃S₂S₁S₀
    -- This might be wrong

    err@(EAdd e0 e1) -> do
        (s1, (e0', t0)) <- algoW e0
        applySt s1 $ do
            (s2, (e1', t1)) <- algoW e1
            -- applySt s2 $ do
            s3 <- exprErr (unify (apply s2 t0) int) err
            s4 <- exprErr (unify (apply s3 t1) int) err
            let comp = s4 `compose` s3 `compose` s2 `compose` s1
            return
                ( comp
                , apply comp (T.EAdd (e0', t0) (e1', t1), int)
                )

    -- \| Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S1
    -- \| τ' = newvar    S₂ = mgu(S₁τ₀, τ₁ → τ')
    -- \| --------------------------------------
    -- \|       Γ ⊢ e₀ e₁ : S₂τ', S₂S₁S₀

    err@(EApp e0 e1) -> do
        fr <- fresh
        (s0, (e0', t0)) <- exprErr (algoW e0) err
        applySt s0 $ do
            (s1, (e1', t1)) <- exprErr (algoW e1) err
            s2 <- exprErr (unify (apply s1 t0) (T.TFun t1 fr)) err
            let t = apply s2 fr
            let comp = s2 `compose` s1 `compose` s0
            return (comp, apply comp (T.EApp (e0', t0) (e1', t1), t))

    -- \| Γ ⊢ e₀ : τ, S₀     S₀Γ, x : S̅₀Γ̅(τ) ⊢ e₁ : τ', S₁
    -- \| ----------------------------------------------
    -- \|        Γ ⊢ let x = e₀ in e₁ : τ', S₁S₀

    -- The bar over S₀ and Γ means "generalize"

    err@(ELet b@(Bind name args e) e1) -> do
        (s1, (_, t0)) <- exprErr (algoW (makeLambda e (coerce args))) err
        bind' <- exprErr (checkBind b) err
        env <- asks vars
        let t' = generalize (apply s1 env) t0
        withBinding (coerce name) t' $ do
            (s2, (e1', t2)) <- algoW e1
            let comp = s2 `compose` s1
            return (comp, apply comp (T.ELet bind' (e1', t2), t2))

    -- \| TODO: Add judgement
    ECase caseExpr injs -> do
        (sub, (e', t)) <- algoW caseExpr
        (subst, injs, ret_t) <- checkCase t injs
        let comp = subst `compose` sub
        let t' = apply comp ret_t
        return (comp, (T.ECase (e', t) injs, t'))

makeLambda :: Exp -> [T.Ident] -> Exp
makeLambda = foldl (flip (EAbs . coerce))

-- | Unify two types producing a new substitution
unify :: T.Type -> T.Type -> Infer Subst
unify t0 t1 | trace ("T0: " <> show t0 <> "\nT1: " <> show t1) False = undefined
unify t0 t1 = do
    case (t0, t1) of
        (T.TFun a b, T.TFun c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s1 `compose` s2
        (T.TVar (T.MkTVar a), t) -> occurs a t
        (t, T.TVar (T.MkTVar b)) -> occurs b t
        (T.TAll _ t, b) -> unify t b
        (a, T.TAll _ t) -> unify a t
        (T.TLit a, T.TLit b) ->
            if a == b
                then return M.empty
                else
                    throwError
                        . unwords
                        $ [ "Can not unify"
                          , "'" <> printTree (T.TLit a) <> "'"
                          , "with"
                          , "'" <> printTree (T.TLit b) <> "'"
                          ]
        (T.TData name t, T.TData name' t') ->
            if name == name' && length t == length t'
                then do
                    xs <- zipWithM unify t t'
                    return $ foldr compose nullSubst xs
                else
                    throwError $
                        unwords
                            [ "T.Type constructor:"
                            , printTree name
                            , "(" <> printTree t <> ")"
                            , "does not match with:"
                            , printTree name'
                            , "(" <> printTree t' <> ")"
                            ]
        (a, b) -> do
            throwError . unwords $
                [ "'" <> printTree a <> "'"
                , "can't be unified with"
                , "'" <> printTree b <> "'"
                ]

{- | Check if a type is contained in another type.
I.E. { a = a -> b } is an unsolvable constraint since there is no substitution
where these are equal
-}
occurs :: T.Ident -> T.Type -> Infer Subst
occurs i t@(T.TVar _) = return (M.singleton i t)
occurs i t =
    if S.member i (free t)
        then
            throwError $
                unwords
                    [ "Occurs check failed, can't unify"
                    , printTree (T.TVar $ T.MkTVar i)
                    , "with"
                    , printTree t
                    ]
        else return $ M.singleton i t

-- | Generalize a type over all free variables in the substitution set
generalize :: Map T.Ident T.Type -> T.Type -> T.Type
generalize env t = go freeVars $ removeForalls t
  where
    freeVars :: [T.Ident]
    freeVars = S.toList $ free t S.\\ free env
    go :: [T.Ident] -> T.Type -> T.Type
    go [] t = t
    go (x : xs) t = T.TAll (T.MkTVar x) (go xs t)
    removeForalls :: T.Type -> T.Type
    removeForalls (T.TAll _ t) = removeForalls t
    removeForalls (T.TFun t1 t2) = T.TFun (removeForalls t1) (removeForalls t2)
    removeForalls t = t

{- | Instantiate a polymorphic type. The free type variables are substituted
with fresh ones.
-}
inst :: T.Type -> Infer T.Type
inst = \case
    T.TAll (T.MkTVar bound) t -> do
        fr <- fresh
        let s = M.singleton bound fr
        apply s <$> inst t
    T.TFun t1 t2 -> T.TFun <$> inst t1 <*> inst t2
    rest -> return rest

-- | Compose two substitution sets
compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (apply m1) m2 `M.union` m1

-- TODO: Split this class into two separate classes, one for free variables
--       and one for applying substitutions

-- | A class representing free variables functions
class FreeVars t where
    -- | Get all free variables from t
    free :: t -> Set T.Ident

    -- | Apply a substitution to t
    apply :: Subst -> t -> t

instance FreeVars T.Type where
    free :: T.Type -> Set T.Ident
    free (T.TVar (T.MkTVar a)) = S.singleton a
    free (T.TAll (T.MkTVar bound) t) = S.singleton bound `S.intersection` free t
    free (T.TLit _) = mempty
    free (T.TFun a b) = free a `S.union` free b
    -- \| Not guaranteed to be correct
    free (T.TData _ a) =
        foldl' (\acc x -> free x `S.union` acc) S.empty a

    apply :: Subst -> T.Type -> T.Type
    apply sub t = do
        case t of
            T.TLit a -> T.TLit a
            T.TVar (T.MkTVar a) -> case M.lookup a sub of
                Nothing -> T.TVar (T.MkTVar $ coerce a)
                Just t -> t
            T.TAll (T.MkTVar i) t -> case M.lookup i sub of
                Nothing -> T.TAll (T.MkTVar i) (apply sub t)
                Just _ -> apply sub t
            T.TFun a b -> T.TFun (apply sub a) (apply sub b)
            T.TData name a -> T.TData name (map (apply sub) a)

instance FreeVars (Map T.Ident T.Type) where
    free :: Map T.Ident T.Type -> Set T.Ident
    free m = foldl' S.union S.empty (map free $ M.elems m)
    apply :: Subst -> Map T.Ident T.Type -> Map T.Ident T.Type
    apply s = M.map (apply s)

instance FreeVars T.ExpT where
    free :: T.ExpT -> Set T.Ident
    free = error "free not implemented for T.Exp"
    apply :: Subst -> T.ExpT -> T.ExpT
    apply s = \case
        (T.EId i, outerT) -> (T.EId i, apply s outerT)
        (T.ELit lit, t) -> (T.ELit lit, apply s t)
        (T.ELet (T.Bind (ident, t1) args e1) e2, t2) ->
            ( T.ELet
                (T.Bind (ident, apply s t1) args (apply s e1))
                (apply s e2)
            , apply s t2
            )
        (T.EApp e1 e2, t) -> (T.EApp (apply s e1) (apply s e2), apply s t)
        (T.EAdd e1 e2, t) -> (T.EAdd (apply s e1) (apply s e2), apply s t)
        (T.EAbs ident e, t1) -> (T.EAbs ident (apply s e), apply s t1)
        (T.ECase e injs, t) -> (T.ECase (apply s e) (apply s injs), apply s t)

instance FreeVars T.Branch where
    free :: T.Branch -> Set T.Ident
    free = undefined
    apply :: Subst -> T.Branch -> T.Branch
    apply s (T.Branch (i, t) e) = T.Branch (i, apply s t) (apply s e)

instance FreeVars [T.Branch] where
    free :: [T.Branch] -> Set T.Ident
    free = foldl' (\acc x -> free x `S.union` acc) mempty
    apply s = map (apply s)

-- | Apply substitutions to the environment.
applySt :: Subst -> Infer a -> Infer a
applySt s = local (\st -> st{vars = apply s (vars st)})

-- | Represents the empty substition set
nullSubst :: Subst
nullSubst = M.empty

-- | Generate a new fresh variable and increment the state counter
fresh :: Infer T.Type
fresh = do
    n <- gets count
    modify (\st -> st{count = n + 1})
    return . T.TVar . T.MkTVar . T.Ident $ show n

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => T.Ident -> T.Type -> m a -> m a
withBinding i p = local (\st -> st{vars = M.insert i p (vars st)})

-- | Run the monadic action with several additional bindings
withBindings :: (Monad m, MonadReader Ctx m) => [(T.Ident, T.Type)] -> m a -> m a
withBindings xs =
    local (\st -> st{vars = foldl' (flip (uncurry M.insert)) (vars st) xs})

-- | Insert a function signature into the environment
insertSig :: T.Ident -> Maybe T.Type -> Infer ()
insertSig i t = modify (\st -> st{sigs = M.insert i t (sigs st)})

-- | Insert a constructor with its data type
insertConstr :: T.Ident -> T.Type -> Infer ()
insertConstr i t =
    modify (\st -> st{constructors = M.insert i t (constructors st)})

-------- PATTERN MATCHING ---------

checkCase :: T.Type -> [Branch] -> Infer (Subst, [T.Branch], T.Type)
checkCase expT injs = do
    (injTs, injs, returns) <- unzip3 <$> mapM inferBranch injs
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
inferBranch :: Branch -> Infer (T.Type, T.Branch, T.Type)
inferBranch (Branch pat expr) = do
    newPat@(pat, branchT) <- inferPattern pat
    newExp@(_, exprT) <- withPattern pat (inferExp expr)
    return (branchT, T.Branch newPat newExp, exprT)

-- return (initT, T.Branch (it, initT) (e, exprT), exprT)

withPattern :: T.Pattern -> Infer a -> Infer a
withPattern p ma = case p of
    T.PVar (x, t) -> withBinding x t ma
    T.PInj _ ps -> foldl' (flip withPattern) ma ps
    T.PLit _ -> ma
    T.PCatch -> ma
    T.PEnum _ -> ma

inferPattern :: Pattern -> Infer (T.Pattern, T.Type)
inferPattern = \case
    PLit lit -> let lt = litType lit in return (T.PLit (toNew lit, lt), lt)
    PInj constr patterns -> do
        t <- gets (M.lookup (coerce constr) . constructors)
        t <- maybeToRightM ("Constructor: " <> printTree constr <> " does not exist") t
        (vs, ret) <- maybeToRightM "Partial pattern match not allowed" (unsnoc $ flattenType t)
        patterns <- mapM inferPattern patterns
        zipWithM_ unify vs (map snd patterns)
        return (T.PInj (coerce constr) (map fst patterns), ret)
    PCatch -> (T.PCatch,) <$> fresh
    PEnum p -> do
        t <- gets (M.lookup (coerce p) . constructors)
        t <- maybeToRightM ("Constructor: " <> printTree p <> " does not exist") t
        return (T.PEnum $ coerce p, t)
    PVar x -> do
        fr <- fresh
        let pvar = T.PVar (coerce x, fr)
        return (pvar, fr)

flattenType :: T.Type -> [T.Type]
flattenType (T.TFun a b) = flattenType a <> flattenType b
flattenType a = [a]

litType :: Lit -> T.Type
litType (LInt _) = int
litType (LChar _) = char

int = T.TLit "Int"
char = T.TLit "Char"

partitionType ::
    Int -> -- Number of parameters to apply
    Type ->
    ([Type], Type)
partitionType = go []
  where
    go acc 0 t = (acc, t)
    go acc i t = case t of
        TAll tvar t' -> second (TAll tvar) $ go acc i t'
        TFun t1 t2 -> go (acc <> [t1]) (i - 1) t2
        _ -> error "Number of parameters and type doesn't match"

exprErr :: Infer a -> Exp -> Infer a
exprErr ma exp =
    catchError ma (\x -> throwError $ x <> " on expression: " <> printTree exp)
