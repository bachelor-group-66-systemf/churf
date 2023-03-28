{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

-- | A module for type checking and inference using algorithm W, Hindley-Milner
module TypeChecker.TypeCheckerHm where

import Auxiliary (maybeToRightM)
import Auxiliary qualified as Aux
import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce (coerce)
import Data.Function (on)
import Data.List (foldl')
import Data.List.Extra (unsnoc)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Grammar.Abs
import Grammar.Print (printTree)
import TypeChecker.TypeCheckerIr qualified as T

initCtx = Ctx mempty
initEnv = Env 0 'a' mempty mempty mempty

run :: Infer a -> Either Error a
run = run' initEnv initCtx

run' :: Env -> Ctx -> Infer a -> Either Error a
run' e c =
    runIdentity
        . runExceptT
        . flip runReaderT c
        . flip evalStateT e
        . runInfer

-- | Type check a program
typecheck :: Program -> Either String (T.Program' Type)
typecheck = onLeft msg . run . checkPrg
  where
    onLeft :: (Error -> String) -> Either Error a -> Either String a
    onLeft f (Left x) = Left $ f x
    onLeft _ (Right x) = Right x

checkPrg :: Program -> Infer (T.Program' Type)
checkPrg (Program bs) = do
    preRun bs
    bs' <- checkDef bs
    return $ T.Program bs'

preRun :: [Def] -> Infer ()
preRun [] = return ()
preRun (x : xs) = case x of
    DSig (Sig n t) -> do
        collect (collectTVars t)
        gets (M.member (coerce n) . sigs)
            >>= flip
                when
                ( uncatchableErr $ Aux.do
                    "Duplicate signatures for function"
                    quote $ printTree n
                )
        insertSig (coerce n) (Just t) >> preRun xs
    DBind (Bind n _ e) -> do
        collect (collectTVars e)
        s <- gets sigs
        case M.lookup (coerce n) s of
            Nothing -> insertSig (coerce n) Nothing >> preRun xs
            Just _ -> preRun xs
    DData d@(Data t _) -> collect (collectTVars t) >> checkData d >> preRun xs

checkDef :: [Def] -> Infer [T.Def' Type]
checkDef [] = return []
checkDef (x : xs) = case x of
    (DBind b) -> do
        b' <- checkBind b
        xs' <- checkDef xs
        return $ T.DBind b' : xs'
    (DData d) -> do
        xs' <- checkDef xs
        return $ T.DData (coerceData d) : xs'
    (DSig _) -> checkDef xs
  where
    coerceData (Data t injs) =
        T.Data t $ map (\(Inj name typ) -> T.Inj (coerce name) typ) injs

checkBind :: Bind -> Infer (T.Bind' Type)
checkBind (Bind name args e) = do
    let lambda = makeLambda e (reverse (coerce args))
    (sub0, (e, lambda_t)) <- inferExp lambda
    s <- gets sigs
    case M.lookup (coerce name) s of
        Just (Just t') -> do
            sab <- unify t' lambda_t
            let fsig = apply (sab `compose` sub0) t'
            sub1 <- liftEither $ runIdentity $ runExceptT $ execStateT (typeEq fsig lambda_t) mempty
            sub2 <- liftEither $ runIdentity $ runExceptT $ execStateT (typeEq lambda_t fsig) mempty
            unless
                (lambda_t == apply sub1 fsig && apply sub2 lambda_t == fsig)
                ( uncatchableErr $ Aux.do
                    "Inferred type"
                    quote $ printTree lambda_t
                    "does not match specified type"
                    quote $ printTree t'
                )
            return $ T.Bind (coerce name, lambda_t) [] (e, lambda_t)
        _ -> do
            insertSig (coerce name) (Just lambda_t)
            return (T.Bind (coerce name, lambda_t) [] (e, lambda_t))

checkData :: Data -> Infer ()
checkData err@(Data typ injs) = do
    (name, tvars) <- go typ
    dataErr (mapM_ (\i -> checkInj i name tvars) injs) err
  where
    go = \case
        TData name typs
            | Right tvars' <- mapM toTVar typs ->
                pure (name, tvars')
        TAll _ _ -> uncatchableErr "Explicit foralls not allowed, for now"
        _ ->
            uncatchableErr $
                unwords ["Bad data type definition: ", printTree typ]

checkInj :: Inj -> UIdent -> [TVar] -> Infer ()
checkInj (Inj c inj_typ) name tvars
    | Right False <- boundTVars tvars inj_typ =
        catchableErr "Unbound type variables"
    | TData name' typs <- returnType inj_typ
    , Right tvars' <- mapM toTVar typs
    , name' == name
    , tvars' == tvars = do
        exist <- existInj (coerce c)
        case exist of
            Just t -> uncatchableErr $ Aux.do
                "Constructor"
                quote $ coerce name
                "with type"
                quote $ printTree t
                "already exist"
            Nothing -> insertInj (coerce c) inj_typ
    | otherwise =
        uncatchableErr $
            unwords
                [ "Bad type constructor: "
                , show name
                , "\nExpected: "
                , printTree . TData name $ map TVar tvars
                , "\nActual: "
                , printTree $ returnType inj_typ
                ]
  where
    boundTVars :: [TVar] -> Type -> Either Error Bool
    boundTVars tvars' = \case
        TAll{} -> uncatchableErr "Explicit foralls not allowed, for now"
        TFun t1 t2 -> do
            t1' <- boundTVars tvars t1
            t2' <- boundTVars tvars t2
            return $ t1' && t2'
        TVar tvar -> return $ tvar `elem` tvars'
        TData _ typs -> and <$> mapM (boundTVars tvars) typs
        TLit _ -> return True
        TEVar _ -> error "TEVar in data type declaration"

toTVar :: Type -> Either Error TVar
toTVar = \case
    TVar tvar -> pure tvar
    _ -> uncatchableErr "Not a type variable"

returnType :: Type -> Type
returnType (TFun _ t2) = returnType t2
returnType a = a

inferExp :: Exp -> Infer (Subst, T.ExpT' Type)
inferExp e = do
    (s, (e', t)) <- algoW e
    let subbed = apply s t
    return (s, (e', subbed))

class CollectTVars a where
    collectTVars :: a -> Set T.Ident

instance CollectTVars Exp where
    collectTVars (EAnn e t) = collectTVars t `S.union` collectTVars e
    collectTVars _ = S.empty

instance CollectTVars Type where
    collectTVars (TVar (MkTVar i)) = S.singleton (coerce i)
    collectTVars (TAll _ t) = collectTVars t
    collectTVars (TFun t1 t2) = (S.union `on` collectTVars) t1 t2
    collectTVars (TData _ ts) =
        foldl' (\acc x -> acc `S.union` collectTVars x) S.empty ts
    collectTVars _ = S.empty

collect :: Set T.Ident -> Infer ()
collect s = modify (\st -> st{takenTypeVars = s `S.union` takenTypeVars st})

algoW :: Exp -> Infer (Subst, T.ExpT' Type)
algoW = \case
    err@(EAnn e t) -> do
        (sub0, (e', t')) <- exprErr (algoW e) err
        sub1 <- unify t t'
        sub2 <- unify t' t
        unless
            (apply sub1 t == t' && apply sub2 t' == t)
            ( uncatchableErr $ Aux.do
                "Annotated type"
                quote $ printTree t
                "does not match inferred type"
                quote $ printTree t'
            )
        let comp = sub2 `compose` sub1 `compose` sub0
        return (comp, apply comp (e', t))

    -- \| ------------------
    -- \|   Γ ⊢ i : Int, ∅

    ELit lit -> return (nullSubst, (T.ELit lit, litType lit))
    -- \| x : σ ∈ Γ   τ = inst(σ)
    -- \| ----------------------
    -- \|     Γ ⊢ x : τ, ∅
    EVar i -> do
        var <- asks vars
        case M.lookup (coerce i) var of
            Just t ->
                inst t >>= \x ->
                    return (nullSubst, (T.EVar $ coerce i, x))
            Nothing -> do
                sig <- gets sigs
                case M.lookup (coerce i) sig of
                    Just (Just t) -> return (nullSubst, (T.EVar $ coerce i, t))
                    Just Nothing -> do
                        fr <- fresh
                        insertSig (coerce i) (Just fr)
                        return (nullSubst, (T.EVar $ coerce i, fr))
                    Nothing ->
                        uncatchableErr $
                            "Unbound variable: "
                                <> printTree i
    EInj i -> do
        constr <- gets injections
        case M.lookup (coerce i) constr of
            Just t -> return (nullSubst, (T.EVar $ coerce i, t))
            Nothing ->
                uncatchableErr $ Aux.do
                    "Constructor:"
                    quote $ printTree i
                    "is not defined"

    -- \| τ = newvar   Γ, x : τ ⊢ e : τ', S
    -- \| ---------------------------------
    -- \|     Γ ⊢ w λx. e : Sτ → τ', S

    err@(EAbs name e) -> do
        fr <- fresh
        withBinding (coerce name) fr $ do
            (s1, (e', t')) <- exprErr (algoW e) err
            let varType = apply s1 fr
            let newArr = TFun varType t'
            return (s1, apply s1 (T.EAbs (coerce name) (e', t'), newArr))

    -- \| Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S₁
    -- \| s₂ = mgu(s₁τ₀, Int)    s₃ = mgu(s₂τ₁, Int)
    -- \| ------------------------------------------
    -- \|        Γ ⊢ e₀ + e₁ : Int, S₃S₂S₁S₀
    -- This might be wrong

    err@(EAdd e0 e1) -> do
        (s1, (e0', t0)) <- algoW e0
        (s2, (e1', t1)) <- algoW e1
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
        (s0, (e0', t0)) <- algoW e0
        (s1, (e1', t1)) <- algoW e1
        s2 <- exprErr (unify (apply s1 t0) (TFun t1 fr)) err
        let t = apply s2 fr
        let comp = s2 `compose` s1 `compose` s0
        return (comp, apply comp (T.EApp (e0', t0) (e1', t1), t))

    -- \| Γ ⊢ e₀ : τ, S₀     S₀Γ, x : S̅₀Γ̅(τ) ⊢ e₁ : τ', S₁
    -- \| ----------------------------------------------
    -- \|        Γ ⊢ let x = e₀ in e₁ : τ', S₁S₀

    -- The bar over S₀ and Γ means "generalize"

    err@(ELet b@(Bind name args e) e1) -> do
        (s1, (_, t0)) <- algoW (makeLambda e (coerce args))
        bind' <- exprErr (checkBind b) err
        env <- asks vars
        let t' = generalize (apply s1 env) t0
        withBinding (coerce name) t' $ do
            (s2, (e1', t2)) <- algoW e1
            let comp = s2 `compose` s1
            return (comp, apply comp (T.ELet bind' (e1', t2), t2))
    ECase caseExpr injs -> do
        (sub, (e', t)) <- algoW caseExpr
        (subst, injs, ret_t) <- checkCase t injs
        let comp = subst `compose` sub
        return (comp, apply comp (T.ECase (e', t) injs, ret_t))
    EAppInf{} -> error "desugar phase failed"

checkCase :: Type -> [Branch] -> Infer (Subst, [T.Branch' Type], Type)
checkCase _ [] = catchableErr "Atleast one case required"
checkCase expT brnchs = do
    (subs, branchTs, injs, returns) <- unzip4 <$> mapM inferBranch brnchs
    let sub0 = composeAll subs
    (sub1, _) <-
        foldM
            ( \(sub, acc) x ->
                (\a -> (a `compose` sub, a `apply` acc)) <$> unify x acc
            )
            (nullSubst, expT)
            branchTs
    (sub2, returns_type) <-
        foldM
            ( \(sub, acc) x ->
                (\a -> (a `compose` sub, a `apply` acc)) <$> unify x acc
            )
            (nullSubst, head returns)
            (tail returns)
    let comp = sub2 `compose` sub1 `compose` sub0
    return (comp, apply comp injs, apply comp returns_type)

inferBranch :: Branch -> Infer (Subst, Type, T.Branch' Type, Type)
inferBranch (Branch pat expr) = do
    newPat@(pat, branchT) <- inferPattern pat
    (sub, newExp@(_, exprT)) <- withPattern pat (algoW expr)
    return
        ( sub
        , apply sub branchT
        , T.Branch (apply sub newPat) (apply sub newExp)
        , apply sub exprT
        )

inferPattern :: Pattern -> Infer (T.Pattern' Type, Type)
inferPattern = \case
    PLit lit -> let lt = litType lit in return (T.PLit (lit, lt), lt)
    PInj constr patterns -> do
        t <- gets (M.lookup (coerce constr) . injections)
        t <-
            maybeToRightM
                ( Error
                    ( Aux.do
                        "Constructor:"
                        quote $ printTree constr
                        "does not exist"
                    )
                    True
                )
                t
        let numArgs = typeLength t - 1
        let (vs, ret) = fromJust (unsnoc $ flattenType t)
        patterns <- mapM inferPattern patterns
        unless
            (length patterns == numArgs)
            ( catchableErr $ Aux.do
                "The constructor"
                quote $ printTree constr
                " should have "
                show numArgs
                " arguments but has been given "
                show (length patterns)
            )
        sub <- composeAll <$> zipWithM unify vs (map snd patterns)
        return
            ( T.PInj (coerce constr) (apply sub (map fst patterns))
            , apply sub ret
            )
    PCatch -> (T.PCatch,) <$> fresh
    PEnum p -> do
        t <- gets (M.lookup (coerce p) . injections)
        t <-
            maybeToRightM
                ( Error
                    ( Aux.do
                        "Constructor:"
                        quote $ printTree p
                        "does not exist"
                    )
                    True
                )
                t
        unless
            (typeLength t == 1)
            ( catchableErr $ Aux.do
                "The constructor"
                quote $ printTree p
                " should have "
                show (typeLength t - 1)
                " arguments but has been given 0"
            )
        let (TData _data _ts) = t -- nasty nasty
        frs <- mapM (const fresh) _ts
        return (T.PEnum $ coerce p, TData _data frs)
    PVar x -> do
        fr <- fresh
        let pvar = T.PVar (coerce x, fr)
        return (pvar, fr)

-- | Unify two types producing a new substitution
unify :: Type -> Type -> Infer Subst
unify t0 t1 =
    case (t0, t1) of
        (TFun a b, TFun c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s1 `compose` s2
        (TVar (MkTVar a), t@(TData _ _)) -> return $ M.singleton (coerce a) t
        (t@(TData _ _), TVar (MkTVar b)) -> return $ M.singleton (coerce b) t
        (TVar (MkTVar a), t) -> occurs (coerce a) t
        (t, TVar (MkTVar b)) -> occurs (coerce b) t
        (TAll _ t, b) -> unify t b
        (a, TAll _ t) -> unify a t
        (TLit a, TLit b) ->
            if a == b
                then return M.empty
                else catchableErr $
                    Aux.do
                        "Can not unify"
                        quote $ printTree (TLit a)
                        "with"
                        quote $ printTree (TLit b)
        (TData name t, TData name' t') ->
            if name == name' && length t == length t'
                then do
                    xs <- zipWithM unify t t'
                    return $ foldr compose nullSubst xs
                else catchableErr $
                    Aux.do
                        "Type constructor:"
                        printTree name
                        quote $ printTree t
                        "does not match with:"
                        printTree name'
                        quote $ printTree t'
        (TEVar a, TEVar b) ->
            if a == b
                then return M.empty
                else catchableErr $
                    Aux.do
                        "Can not unify"
                        quote $ printTree (TEVar a)
                        "with"
                        quote $ printTree (TEVar b)
        (a, b) -> do
            catchableErr $
                Aux.do
                    "Can not unify"
                    quote $ printTree a
                    "with"
                    quote $ printTree b

{- | Check if a type is contained in another type.
I.E. { a = a -> b } is an unsolvable constraint since there is no substitution
where these are equal
-}
occurs :: T.Ident -> Type -> Infer Subst
occurs i t@(TVar _) = return (M.singleton i t)
occurs i t =
    if S.member i (free t)
        then
            catchableErr
                ( Aux.do
                    "Occurs check failed, can't unify"
                    quote $ printTree (TVar $ MkTVar (coerce i))
                    "with"
                    quote $ printTree t
                )
        else return $ M.singleton i t

{- | Generalize a type over all free variables in the substitution set
     Used for let bindings to allow expression that do not type check in
     equivalent lambda expressions:
        Type checks: let f = \x. x in (f True, f 'a')
        Does not type check: (\f. (f True, f 'a')) (\x. x)
-}
generalize :: Map T.Ident Type -> Type -> Type
generalize env t = go (S.toList $ free t S.\\ free env) (removeForalls t)
  where
    go :: [T.Ident] -> Type -> Type
    go [] t = t
    go (x : xs) t = TAll (MkTVar (coerce x)) (go xs t)
    removeForalls :: Type -> Type
    removeForalls (TAll _ t) = removeForalls t
    removeForalls (TFun t1 t2) = TFun (removeForalls t1) (removeForalls t2)
    removeForalls t = t

{- | Instantiate a polymorphic type. The free type variables are substituted
with fresh ones.
-}
inst :: Type -> Infer Type
inst = \case
    TAll (MkTVar bound) t -> do
        fr <- fresh
        let s = M.singleton (coerce bound) fr
        apply s <$> inst t
    TFun t1 t2 -> TFun <$> inst t1 <*> inst t2
    rest -> return rest

-- | Generate a new fresh variable
fresh :: Infer Type
fresh = do
    c <- gets nextChar
    n <- gets count
    taken <- gets takenTypeVars
    if c == 'z'
        then do
            modify (\st -> st{count = succ (count st), nextChar = 'a'})
        else modify (\st -> st{nextChar = next (nextChar st)})
    if coerce [c] `S.member` taken
        then do
            fresh
        else
            if n == 0
                then return . TVar . MkTVar $ LIdent [c]
                else return . TVar . MkTVar . LIdent $ c : show n
  where
    next :: Char -> Char
    next 'z' = 'a'
    next a = succ a

-- | A class for substitutions
class SubstType t where
    -- | Apply a substitution to t
    apply :: Subst -> t -> t

class FreeVars t where
    -- | Get all free variables from t
    free :: t -> Set T.Ident

instance FreeVars Type where
    free :: Type -> Set T.Ident
    free (TVar (MkTVar a)) = S.singleton (coerce a)
    free (TAll (MkTVar bound) t) =
        S.singleton (coerce bound) `S.intersection` free t
    free (TLit _) = mempty
    free (TFun a b) = free a `S.union` free b
    free (TData _ a) = free a
    free (TEVar _) = S.empty

instance FreeVars a => FreeVars [a] where
    free = let f acc x = acc `S.union` free x in foldl' f S.empty

instance SubstType Type where
    apply :: Subst -> Type -> Type
    apply sub t = do
        case t of
            TLit a -> TLit a
            TVar (MkTVar a) -> case M.lookup (coerce a) sub of
                Nothing -> TVar (MkTVar $ coerce a)
                Just t -> t
            TAll (MkTVar i) t -> case M.lookup (coerce i) sub of
                Nothing -> TAll (MkTVar i) (apply sub t)
                Just _ -> apply sub t
            TFun a b -> TFun (apply sub a) (apply sub b)
            TData name a -> TData name (apply sub a)
            TEVar (MkTEVar a) -> case M.lookup (coerce a) sub of
                Nothing -> TEVar (MkTEVar a)
                Just t -> t

instance FreeVars (Map T.Ident Type) where
    free :: Map T.Ident Type -> Set T.Ident
    free = free . M.elems

instance SubstType (Map T.Ident Type) where
    apply :: Subst -> Map T.Ident Type -> Map T.Ident Type
    apply = M.map . apply

instance SubstType (T.Exp' Type) where
    apply s = \case
        T.EVar i -> T.EVar i
        T.ELit lit -> T.ELit lit
        T.ELet (T.Bind (ident, t1) args e1) e2 ->
            T.ELet
                (T.Bind (ident, apply s t1) args (apply s e1))
                (apply s e2)
        T.EApp e1 e2 -> T.EApp (apply s e1) (apply s e2)
        T.EAdd e1 e2 -> T.EAdd (apply s e1) (apply s e2)
        T.EAbs ident e -> T.EAbs ident (apply s e)
        T.ECase e brnch -> T.ECase (apply s e) (apply s brnch)
        T.EInj i -> T.EInj i

instance SubstType (T.Branch' Type) where
    apply s (T.Branch (i, t) e) = T.Branch (apply s i, apply s t) (apply s e)

instance SubstType (T.Pattern' Type) where
    apply s = \case
        T.PVar (iden, t) -> T.PVar (iden, apply s t)
        T.PLit (lit, t) -> T.PLit (lit, apply s t)
        T.PInj i ps -> T.PInj i $ apply s ps
        T.PCatch -> T.PCatch
        T.PEnum i -> T.PEnum i

instance SubstType a => SubstType [a] where
    apply s = map (apply s)

instance (SubstType a, SubstType b) => SubstType (a, b) where
    apply s (a, b) = (apply s a, apply s b)

instance SubstType (T.Id' Type) where
    apply s (name, t) = (name, apply s t)

-- | Represents the empty substition set
nullSubst :: Subst
nullSubst = M.empty

-- | Compose two substitution sets
compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (apply m1) m2 `M.union` m1

-- | Compose a list of substitution sets into one
composeAll :: [Subst] -> Subst
composeAll = foldl' compose nullSubst

{- | Convert a function with arguments to its pointfree version
> makeLambda (add x y = x + y) = add = \x. \y. x + y
-}
makeLambda :: Exp -> [T.Ident] -> Exp
makeLambda = foldl (flip (EAbs . coerce))

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => T.Ident -> Type -> m a -> m a
withBinding i p = local (\st -> st{vars = M.insert i p (vars st)})

-- | Run the monadic action with several additional bindings
withBindings :: (Monad m, MonadReader Ctx m) => [(T.Ident, Type)] -> m a -> m a
withBindings xs =
    local (\st -> st{vars = foldl' (flip (uncurry M.insert)) (vars st) xs})

-- | Run the monadic action with a pattern
withPattern :: (Monad m, MonadReader Ctx m) => T.Pattern' Type -> m a -> m a
withPattern p ma = case p of
    T.PVar (x, t) -> withBinding x t ma
    T.PInj _ ps -> foldl' (flip withPattern) ma ps
    T.PLit _ -> ma
    T.PCatch -> ma
    T.PEnum _ -> ma

-- | Insert a function signature into the environment
insertSig :: T.Ident -> Maybe Type -> Infer ()
insertSig i t = modify (\st -> st{sigs = M.insert i t (sigs st)})

-- | Insert a constructor into the start with its type
insertInj :: T.Ident -> Type -> Infer ()
insertInj i t =
    modify (\st -> st{injections = M.insert i t (injections st)})

{- | Check if an injection (constructor of data type)
with an equivalent name has been declared already
-}
existInj :: T.Ident -> Infer (Maybe Type)
existInj n = gets (M.lookup n . injections)

flattenType :: Type -> [Type]
flattenType (TFun a b) = flattenType a <> flattenType b
flattenType a = [a]

typeLength :: Type -> Int
typeLength (TFun _ b) = 1 + typeLength b
typeLength _ = 1

litType :: Lit -> Type
litType (LInt _) = int
litType (LChar _) = char

int = TLit "Int"
char = TLit "Char"

typeEq :: Type -> Type -> StateT Subst (ExceptT Error Identity) ()
typeEq (TVar (MkTVar a)) t@(TVar _) = do
    st <- get
    case M.lookup (coerce a) st of
        Nothing -> put $ M.insert (coerce a) t st
        Just t' ->
            unless
                (t == t')
                ( catchableErr $ Aux.do
                    quote $ printTree t
                    "does not match with"
                    quote $ printTree t'
                )
typeEq (TFun l r) (TFun l' r') = typeEq l l' *> typeEq r r'
typeEq (TAll _ l) (TAll _ r) = typeEq l r
typeEq t@(TLit a) t'@(TLit b) =
    unless
        (a == b)
        ( catchableErr $ Aux.do
            quote $ printTree t
            "does not match with"
            quote $ printTree t'
        )
typeEq t@(TData nameL tL) t'@(TData nameR tR) = do
    unless
        (nameL == nameR)
        ( catchableErr $ Aux.do
            quote $ printTree t
            "does not match with"
            quote $ printTree t'
        )
    zipWithM_ typeEq tL tR
typeEq t@(TEVar _) t'@(TEVar _) =
    catchableErr $ Aux.do
        quote $ printTree t
        "does not match with"
        quote $ printTree t'
typeEq t t' = catchableErr $ Aux.do
    quote $ printTree t
    "does not match with"
    quote $ printTree t'

{- | Catch an error if possible and add the given
expression as addition to the error message
-}
exprErr :: (Monad m, MonadError Error m) => m a -> Exp -> m a
exprErr ma exp =
    catchError
        ma
        ( \x ->
            if x.catchable
                then
                    throwError
                        ( x
                            { msg =
                                x.msg
                                    <> " in expression: \n"
                                    <> printTree exp
                            , catchable = False
                            }
                        )
                else throwError x
        )

{- | Catch an error if possible and add the given
data as addition to the error message
-}
dataErr :: Infer a -> Data -> Infer a
dataErr ma d =
    catchError
        ma
        ( \x ->
            if x.catchable
                then
                    throwError
                        ( x
                            { msg =
                                x.msg
                                    <> " in data: \n"
                                    <> printTree d
                            }
                        )
                else throwError (x{catchable = False})
        )

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 =
    foldl'
        ( \(as, bs, cs, ds) (a, b, c, d) ->
            (as ++ [a], bs ++ [b], cs ++ [c], ds ++ [d])
        )
        ([], [], [], [])

newtype Ctx = Ctx {vars :: Map T.Ident Type}
    deriving (Show)

data Env = Env
    { count :: Int
    , nextChar :: Char
    , sigs :: Map T.Ident (Maybe Type)
    , injections :: Map T.Ident Type
    , takenTypeVars :: Set T.Ident
    }
    deriving (Show)

data Error = Error {msg :: String, catchable :: Bool}
    deriving (Show)
type Subst = Map T.Ident Type

newtype Infer a = Infer {runInfer :: StateT Env (ReaderT Ctx (ExceptT Error Identity)) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadError Error, MonadState Env)

catchableErr :: MonadError Error m => String -> m a
catchableErr msg = throwError $ Error msg True

uncatchableErr :: MonadError Error m => String -> m a
uncatchableErr msg = throwError $ Error msg False

quote :: String -> String
quote s = "'" ++ s ++ "'"
