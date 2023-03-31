{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

-- | A module for type checking and inference using algorithm W, Hindley-Milner
module TypeChecker.TypeCheckerHm where

import Auxiliary (int, litType, maybeToRightM, unzip4)
import Auxiliary qualified as Aux
import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce (coerce)
import Data.Function (on)
import Data.List (foldl', intercalate)
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

-- TODO: Disallow mutual recursion

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
    (subs, bs) <- checkDef bs
    ctrace "SUBS" $ unionSubsts subs
    return $ T.Program bs

preRun :: [Def] -> Infer ()
preRun [] = return ()
preRun (x : xs) = case x of
    DSig (Sig n t) -> do
        collect (collectTVars t)
        s <- gets (M.keys . sigs)
        duplicateDecl n s $ Aux.do
            "Multiple signatures of function"
            quote $ printTree n
        insertSig (coerce n) t
        preRun xs
    DBind (Bind n _ e) -> do
        s <- gets (S.toList . declaredBinds)
        duplicateDecl n s $ Aux.do
            "Multiple declarations of function"
            quote $ printTree n
        collect (collectTVars e)
        insertBind $ coerce n
        sigs <- gets sigs
        case M.lookup (coerce n) sigs of
            Nothing -> do
                fr <- fresh
                insertSig (coerce n) fr
                preRun xs
            Just _ -> preRun xs
    DData d@(Data t _) -> collect (collectTVars t) >> checkData d >> preRun xs
  where
    -- Check if function body / signature has been declared already
    duplicateDecl :: (Monad m, MonadError Error m) => LIdent -> [T.Ident] -> String -> m ()
    duplicateDecl n env msg = when (coerce n `elem` env) (uncatchableErr msg)

checkDef :: [Def] -> Infer ([Subst], [T.Def' Type])
checkDef [] = return ([], [])
checkDef (x : xs) = case x of
    (DBind b) -> do
        (sub0, b') <- checkBind b
        (sub1, xs') <- checkDef xs
        return (sub1 ++ sub0, T.DBind b' : xs')
    (DData d) -> do
        (sub, xs') <- checkDef xs
        return (sub, T.DData (coerceData d) : xs')
    (DSig _) -> checkDef xs
  where
    coerceData (Data t injs) =
        T.Data t $ map (\(Inj name typ) -> T.Inj (coerce name) typ) injs

checkBind :: Bind -> Infer ([Subst], T.Bind' Type)
checkBind bind@(Bind name args e) = do
    let lambda = makeLambda e (reverse (coerce args))
    (sub0, (e, lambda_t)) <- inferExp lambda
    s <- gets sigs
    case M.lookup (coerce name) s of
        Just t' -> do
            sub1 <- bindErr (unify t' lambda_t) bind
            ctrace "SUB0" sub0
            ctrace "SUB1" sub1
            return ([sub1, sub0], T.Bind (coerce name, t') [] (e, lambda_t))
        _ -> error "First pass through failed to add function to env"

checkData :: (MonadState Env m, Monad m, MonadError Error m) => Data -> m ()
checkData err@(Data typ injs) = do
    (name, tvars) <- go typ
    dataErr (mapM_ (\i -> checkInj i name tvars) injs) err
  where
    go = \case
        TData name typs
            | Right tvars' <- mapM toTVar typs ->
                pure (name, tvars')
        TAll _ _ -> uncatchableErr "Explicit forall not allowed, for now"
        _ ->
            uncatchableErr $
                unwords ["Bad data type definition: ", printTree typ]

checkInj :: (MonadError Error m, MonadState Env m, Monad m) => Inj -> UIdent -> [TVar] -> m ()
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
        TAll{} -> uncatchableErr "Explicit forall not allowed, for now"
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
    subbed <- apply s t
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
        let comp = sub2 `compose` sub1 `compose` sub0
        et <- apply comp (e', t)
        return (comp, et)

    -- \| ------------------
    -- \|   Γ ⊢ i : Int, ∅

    ELit lit -> return (nullSubst, (T.ELit lit, litType lit))
    -- \| x : σ ∈ Γ   τ = inst(σ)
    -- \| ----------------------
    -- \|     Γ ⊢ x : τ, ∅
    EVar (LIdent i) -> do
        var <- asks vars
        case M.lookup (coerce i) var of
            Just t ->
                inst t >>= \x ->
                    return (nullSubst, (T.EVar $ coerce i, x))
            Nothing -> do
                sig <- gets sigs
                case M.lookup (coerce i) sig of
                    Just t -> return (nullSubst, (T.EVar $ coerce i, t))
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
            varType <- apply s1 fr
            let newArr = TFun varType t'
            eabs <- apply s1 (T.EAbs (coerce name) (e', t'), newArr)
            return (s1, eabs)

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
        applySt s0 $ do
            modify (\st -> st{sigs = apply s0 st.sigs})
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
        (_, bind') <- exprErr (checkBind b) err
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
    PCatch -> (T.PCatch,) <$> fresh
    PVar x -> do
        fr <- fresh
        let pvar = T.PVar (coerce x, fr)
        return (pvar, fr)
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

-- | Unify two types producing a new substitution
unify :: Type -> Type -> Infer Subst
unify t0 t1 =
    case (t0, t1) of
        (TFun a b, TFun c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s2 `compose` s1
        (TVar (MkTVar a), t@(TData _ _)) -> return $ coerce $ M.singleton (coerce a) t
        (t@(TData _ _), TVar (MkTVar b)) -> return $ coerce $ M.singleton (coerce b) t
        (TVar (MkTVar a), t) -> occurs (coerce a) t
        (t, TVar (MkTVar b)) -> occurs (coerce b) t
        (TAll _ t, b) -> unify t b
        (a, TAll _ t) -> unify a t
        (TLit a, TLit b) ->
            if a == b
                then return nullSubst
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
                then return nullSubst
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
occurs i t@(TVar _) = return (coerce $ M.singleton i t)
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
        else return $ coerce $ M.singleton i t

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
        let s = coerce $ M.singleton (coerce bound) fr
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

skolemize :: Type -> Type
skolemize (TVar (MkTVar a)) = TEVar $ MkTEVar a
skolemize (TAll x t) = TAll x (skolemize t)
skolemize (TFun t1 t2) = (TFun `on` skolemize) t1 t2
skolemize t = t

-- | A class for substitutions
class SubstType t where
    -- | Apply a substitution to t
    -- apply :: MonadError e m => Subst -> t -> m t
    apply :: Subst -> t -> Infer t

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
    apply sub@(Subst s) t = do
        case t of
            TLit a -> return $ TLit a
            TVar (MkTVar a) -> case M.lookup (coerce a) s of
                Nothing -> return $ TVar (MkTVar $ coerce a)
                Just t -> return $ t
            TAll (MkTVar i) t -> case M.lookup (coerce i) s of
                Nothing -> TAll (MkTVar i) <$> apply sub t
                Just _ -> apply sub t
            TFun a b -> TFun <$> apply sub a <*> apply sub b
            TData name a -> TData name <$> apply sub a
            TEVar (MkTEVar a) -> case M.lookup (coerce a) s of
                Nothing -> return $ TEVar (MkTEVar a)
                Just t -> return $ t

instance FreeVars (Map T.Ident Type) where
    free :: Map T.Ident Type -> Set T.Ident
    free = free . M.elems

instance SubstType (Map T.Ident Type) where
    apply s = undefined -- M.map (apply s)

instance SubstType Subst where
    apply s@(Subst m1) (Subst m2) = do
        let both = M.keys $ M.intersection m1 m2
        case both of
            [] -> Subst <$> apply s m2
            xs -> do
                sub0 <- apply s m2
                sub1 <- loop xs m1 m2
                apply sub1 (Subst sub0)
      where
        loop [] _ _ = return nullSubst
        loop (x : xs) m1 m2 = do
            let k1 = m1 M.! x
            let k2 = m2 M.! x
            sub <- unify k1 k2
            subs <- loop xs m1 m2
            return $ sub `compose` subs

-- Subst $ M.map (apply s) m2

instance SubstType (T.ExpT' Type) where
    apply s (e, t) = (apply s e, apply s t)

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

instance SubstType (T.Def' Type) where
    apply s = \case
        T.DBind (T.Bind name args e) -> T.DBind $ T.Bind (apply s name) (apply s args) (apply s e)
        d -> d

instance SubstType (T.Branch' Type) where
    apply s (T.Branch (i, t) e) = T.Branch (apply s i, apply s t) (apply s e)

instance SubstType (T.Pattern' Type) where
    apply s = \case
        T.PVar (iden, t) -> T.PVar (iden, apply s t)
        T.PLit (lit, t) -> T.PLit (lit, apply s t)
        T.PInj i ps -> T.PInj i $ apply s ps
        T.PCatch -> T.PCatch
        T.PEnum i -> T.PEnum i

instance SubstType (T.Pattern' Type, Type) where
    apply s (p, t) = (apply s p, apply s t)

instance SubstType a => SubstType [a] where
    apply s = map (apply s)

instance SubstType (T.Id' Type) where
    apply s (name, t) = (name, apply s t)

-- | Represents the empty substition set
nullSubst :: Subst
nullSubst = Subst mempty

-- | Compose two substitution sets
compose :: Subst -> Subst -> Subst
compose m1 m2 = Subst $ M.map (apply $ coerce m1) (coerce m2) `M.union` coerce m1

-- | Compose a list of substitution sets into one
composeAll :: [Subst] -> Subst
composeAll = foldl' compose nullSubst

unionSubsts :: [Subst] -> Subst
unionSubsts = Subst . foldl' M.union M.empty . map coerce

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
insertSig :: T.Ident -> Type -> Infer ()
insertSig i t = modify (\st -> st{sigs = M.insert i t (sigs st)})

insertBind :: T.Ident -> Infer ()
insertBind i = modify (\st -> st{declaredBinds = S.insert i st.declaredBinds})

-- | Insert a constructor into the start with its type
insertInj :: (Monad m, MonadState Env m) => T.Ident -> Type -> m ()
insertInj i t =
    modify (\st -> st{injections = M.insert i t (injections st)})

applySt :: Subst -> Infer a -> Infer a
applySt s = local (\st -> st{vars = apply s st.vars})

{- | Check if an injection (constructor of data type)
with an equivalent name has been declared already
-}
existInj :: (Monad m, MonadState Env m) => T.Ident -> m (Maybe Type)
existInj n = gets (M.lookup n . injections)

delim :: Char
delim = '_'
prefix :: Char
prefix = '$'

flattenType :: Type -> [Type]
flattenType (TFun a b) = flattenType a <> flattenType b
flattenType a = [a]

typeLength :: Type -> Int
typeLength (TFun _ b) = 1 + typeLength b
typeLength _ = 1

{- | Catch an error if possible and add the given
expression as addition to the error message
-}
exprErr :: (Monad m, MonadError Error m) => m a -> Exp -> m a
exprErr ma exp =
    catchError
        ma
        ( \err ->
            if err.catchable
                then
                    throwError
                        ( err
                            { msg =
                                err.msg
                                    <> " in expression: \n"
                                    <> printTree exp
                            , catchable = False
                            }
                        )
                else throwError err
        )

bindErr :: (Monad m, MonadError Error m) => m a -> Bind -> m a
bindErr ma bind =
    catchError
        ma
        ( \err ->
            if err.catchable
                then
                    throwError
                        ( err
                            { msg =
                                err.msg
                                    <> " in function: \n"
                                    <> printTree bind
                            , catchable = False
                            }
                        )
                else throwError err
        )

{- | Catch an error if possible and add the given
data as addition to the error message
-}
dataErr :: (MonadError Error m, Monad m) => m a -> Data -> m a
dataErr ma d =
    catchError
        ma
        ( \err ->
            if err.catchable
                then
                    throwError
                        ( err
                            { msg =
                                err.msg
                                    <> " in data: \n"
                                    <> printTree d
                            }
                        )
                else throwError (err{catchable = False})
        )

initCtx = Ctx mempty
initEnv = Env 0 'a' mempty mempty mempty mempty

run :: Infer a -> Either Error a
run = run' initEnv initCtx

run' :: Env -> Ctx -> Infer a -> Either Error a
run' e c =
    runIdentity
        . runExceptT
        . flip runReaderT c
        . flip evalStateT e
        . runInfer

newtype Ctx = Ctx {vars :: Map T.Ident Type}
    deriving (Show)

data Env = Env
    { count :: Int
    , nextChar :: Char
    , sigs :: Map T.Ident Type
    , takenTypeVars :: Set T.Ident
    , injections :: Map T.Ident Type
    , declaredBinds :: Set T.Ident
    }
    deriving (Show)

data Error = Error {msg :: String, catchable :: Bool}
    deriving (Show)

newtype Subst = Subst (Map T.Ident Type)

instance Show Subst where
    show (Subst s) = "[" ++ let xs = (map (\(a, b) -> printTree a ++ " = " ++ printTree b) $ M.toList s) in intercalate " | " xs ++ "]"

newtype Infer a = Infer {runInfer :: StateT Env (ReaderT Ctx (ExceptT Error Identity)) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadError Error, MonadState Env)

catchableErr :: MonadError Error m => String -> m a
catchableErr msg = throwError $ Error msg True

uncatchableErr :: MonadError Error m => String -> m a
uncatchableErr msg = throwError $ Error msg False

quote :: String -> String
quote s = "'" ++ s ++ "'"

ctrace :: (Monad m, Show a) => String -> a -> m ()
ctrace str a = trace (str ++ ": " ++ show a) pure ()
