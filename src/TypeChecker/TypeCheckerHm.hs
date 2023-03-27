{-# LANGUAGE LambdaCase #-}
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
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.List (foldl')
import Data.List.Extra (unsnoc)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Grammar.Abs
import Grammar.Print (printTree)
import TypeChecker.TypeCheckerIr qualified as T

initCtx = Ctx mempty
initEnv = Env 0 'a' mempty mempty mempty

run :: Infer a -> Either Error a
run = runC initEnv initCtx

runC :: Env -> Ctx -> Infer a -> Either Error a
runC e c =
    runIdentity
        . runExceptT
        . flip runReaderT c
        . flip evalStateT e
        . runInfer

typecheck :: Program -> Either Error (T.Program' Type)
typecheck = run . checkPrg

checkData :: Data -> Infer ()
checkData (Data typ injs) = do
    (name, tvars) <- go typ
    mapM_ (\i -> typecheckInj i name tvars) injs
  where
    go = \case
        TData name typs
            | Right tvars' <- mapM toTVar typs ->
                pure (name, tvars')
        TAll _ _ -> throwError "Explicit foralls not allowed, for now"
        _ -> throwError $ unwords ["Bad data type definition: ", printTree typ]

typecheckInj :: Inj -> UIdent -> [TVar] -> Infer ()
typecheckInj (Inj c inj_typ) name tvars
    | Right False <- boundTVars tvars inj_typ =
        throwError "Unbound type variables"
    | TData name' typs <- returnType inj_typ
    , Right tvars' <- mapM toTVar typs
    , name' == name
    , tvars' == tvars = do
        exist <- existInj (coerce c)
        case exist of
            Just t -> throwError $ Aux.do
                "Constructor"
                quote $ coerce name
                "with type"
                quote $ printTree t
                "already exist"
            Nothing -> insertInj (coerce c) inj_typ
    | otherwise =
        throwError $
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
        TAll{} -> throwError "Explicit foralls not allowed, for now"
        TFun t1 t2 -> do
            t1' <- boundTVars tvars t1
            t2' <- boundTVars tvars t2
            return $ t1' && t2'
        TVar tvar -> return $ tvar `elem` tvars'
        TData _ typs -> and <$> mapM (boundTVars tvars) typs
        TLit _ -> return True
        TEVar _ -> error "TEVar in data type declaration"

toTVar :: Type -> Either String TVar
toTVar = \case
    TVar tvar -> pure tvar
    _ -> throwError "Not a type variable"

returnType :: Type -> Type
returnType (TFun _ t2) = returnType t2
returnType a = a

checkPrg :: Program -> Infer (T.Program' Type)
checkPrg (Program bs) = do
    preRun bs
    bs' <- checkDef bs
    return $ T.Program bs'

preRun :: [Def] -> Infer ()
preRun [] = return ()
preRun (x : xs) = case x of
    DSig (Sig n t) -> do
        collect (collectTypeVars t)
        gets (M.member (coerce n) . sigs)
            >>= flip
                when
                ( throwError $ Aux.do
                    "Duplicate signatures for function"
                    quote $ printTree n
                )
        insertSig (coerce n) (Just t) >> preRun xs
    DBind (Bind n _ e) -> do
        collect (collectTypeVars e)
        s <- gets sigs
        case M.lookup (coerce n) s of
            Nothing -> insertSig (coerce n) Nothing >> preRun xs
            Just _ -> preRun xs
    DData d@(Data t _) -> collect (collectTypeVars t) >> checkData d >> preRun xs

checkDef :: [Def] -> Infer [T.Def' Type]
checkDef [] = return []
checkDef (x : xs) = case x of
    (DBind b) -> do
        b' <- checkBind b
        fmap (T.DBind b' :) (checkDef xs)
    (DData d) -> fmap (T.DData (coerceData d) :) (checkDef xs)
    (DSig _) -> checkDef xs
  where
    coerceData (Data t injs) =
        T.Data t $ map (\(Inj name typ) -> T.Inj (coerce name) typ) injs

checkBind :: Bind -> Infer (T.Bind' Type)
checkBind (Bind name args e) = do
    let lambda = makeLambda e (reverse (coerce args))
    e@(_, args_t) <- inferExp lambda
    s <- gets sigs
    case M.lookup (coerce name) s of
        Just (Just t') -> do
            unless
                (args_t `typeEq` t')
                ( throwError $ Aux.do
                    "Inferred type"
                    quote $ printTree args_t
                    "does not match specified type"
                    quote $ printTree t'
                )
            return $ T.Bind (coerce name, t') [] e
        _ -> do
            insertSig (coerce name) (Just args_t)
            return (T.Bind (coerce name, args_t) [] e)

typeEq :: Type -> Type -> Bool
typeEq (TFun l r) (TFun l' r') = typeEq l l' && typeEq r r'
typeEq (TLit a) (TLit b) = a == b
typeEq (TData name a) (TData name' b) =
    length a == length b
        && name == name'
        && and (zipWith typeEq a b)
typeEq (TAll _ t1) t2 = t1 `typeEq` t2
typeEq t1 (TAll _ t2) = t1 `typeEq` t2
typeEq (TVar _) (TVar _) = True
typeEq _ _ = False

skolemize :: Type -> Type
skolemize (TVar (MkTVar a)) = TEVar (MkTEVar $ coerce a)
skolemize (TAll x t) = TAll x (skolemize t)
skolemize (TFun t1 t2) = (TFun `on` skolemize) t1 t2
skolemize t = t

isMoreSpecificOrEq :: Type -> Type -> Bool
isMoreSpecificOrEq t1 (TAll _ t2) = isMoreSpecificOrEq t1 t2
isMoreSpecificOrEq (TFun a b) (TFun c d) =
    isMoreSpecificOrEq a c && isMoreSpecificOrEq b d
isMoreSpecificOrEq (TData n1 ts1) (TData n2 ts2) =
    n1 == n2
        && length ts1 == length ts2
        && and (zipWith isMoreSpecificOrEq ts1 ts2)
isMoreSpecificOrEq _ (TVar _) = True
isMoreSpecificOrEq a b = a == b

isPoly :: Type -> Bool
isPoly (TAll _ _) = True
isPoly (TVar _) = True
isPoly _ = False

inferExp :: Exp -> Infer (T.ExpT' Type)
inferExp e = do
    (s, (e', t)) <- algoW e
    let subbed = apply s t
    return $ second (const subbed) (e', t)

class CollectTVars a where
    collectTypeVars :: a -> Set T.Ident

instance CollectTVars Exp where
    collectTypeVars (EAnn e t) = collectTypeVars t `S.union` collectTypeVars e
    collectTypeVars _ = S.empty

instance CollectTVars Type where
    collectTypeVars (TVar (MkTVar i)) = S.singleton (coerce i)
    collectTypeVars (TAll _ t) = collectTypeVars t
    collectTypeVars (TFun t1 t2) = (S.union `on` collectTypeVars) t1 t2
    collectTypeVars (TData _ ts) = foldl' (\acc x -> acc `S.union` collectTypeVars x) S.empty ts
    collectTypeVars _ = S.empty

collect :: Set T.Ident -> Infer ()
collect s = modify (\st -> st{takenTypeVars = s `S.union` takenTypeVars st})

algoW :: Exp -> Infer (Subst, T.ExpT' Type)
algoW = \case
    err@(EAnn e t) -> do
        (s1, (e', t')) <- exprErr (algoW e) err
        unless
            (t `isMoreSpecificOrEq` t')
            ( throwError $
                unwords
                    [ "Annotated type:"
                    , printTree t
                    , "does not match inferred type:"
                    , printTree t'
                    ]
            )
        s2 <- exprErr (unify t t') err
        let comp = s2 `compose` s1
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
            Just t -> inst t >>= \x -> return (nullSubst, (T.EVar $ coerce i, x))
            Nothing -> do
                sig <- gets sigs
                case M.lookup (coerce i) sig of
                    Just (Just t) -> return (nullSubst, (T.EVar $ coerce i, t))
                    Just Nothing -> do
                        fr <- fresh
                        insertSig (coerce i) (Just fr)
                        return (nullSubst, (T.EVar $ coerce i, fr))
                    Nothing -> throwError $ "Unbound variable: " <> printTree i
    EInj i -> do
        constr <- gets injections
        case M.lookup (coerce i) constr of
            Just t -> return (nullSubst, (T.EVar $ coerce i, t))
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
                let newArr = TFun varType t'
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

    -- \| TODO: Add judgement
    ECase caseExpr injs -> do
        (sub, (e', t)) <- algoW caseExpr
        (subst, injs, ret_t) <- checkCase t injs
        let comp = subst `compose` sub
        return (comp, apply comp (T.ECase (e', t) injs, ret_t))

makeLambda :: Exp -> [T.Ident] -> Exp
makeLambda = foldl (flip (EAbs . coerce))

-- | Unify two types producing a new substitution
unify :: Type -> Type -> Infer Subst
unify t0 t1 = do
    case (t0, t1) of
        (TFun a b, TFun c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s1 `compose` s2
        ----------- TODO: BE CAREFUL!!!! THIS IS PROBABLY WRONG!!! -----------
        (TVar (T.MkTVar a), t@(TData _ _)) -> return $ M.singleton (coerce a) t
        (t@(TData _ _), TVar (T.MkTVar b)) -> return $ M.singleton (coerce b) t
        -------------------------------------------------------------------
        (TVar (T.MkTVar a), t) -> occurs (coerce a) t
        (t, TVar (T.MkTVar b)) -> occurs (coerce b) t
        (TAll _ t, b) -> unify t b
        (a, TAll _ t) -> unify a t
        (TLit a, TLit b) ->
            if a == b
                then return M.empty
                else throwError $
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
                else throwError $
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
                else throwError $
                    Aux.do
                        "Can not unify"
                        quote $ printTree (TEVar a)
                        "with"
                        quote $ printTree (TEVar b)
        (a, b) -> do
            throwError $
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
        then throwError $
            Aux.do
                "Occurs check failed, can't unify"
                quote $ printTree (TVar $ T.MkTVar (coerce i))
                "with"
                quote $ printTree t
        else return $ M.singleton i t

-- | Generalize a type over all free variables in the substitution set
generalize :: Map T.Ident Type -> Type -> Type
generalize env t = go (S.toList $ free t S.\\ free env) (removeForalls t)
  where
    go :: [T.Ident] -> Type -> Type
    go [] t = t
    go (x : xs) t = TAll (T.MkTVar (coerce x)) (go xs t)
    removeForalls :: Type -> Type
    removeForalls (TAll _ t) = removeForalls t
    removeForalls (TFun t1 t2) = TFun (removeForalls t1) (removeForalls t2)
    removeForalls t = t

{- | Instantiate a polymorphic type. The free type variables are substituted
with fresh ones.
-}
inst :: Type -> Infer Type
inst = \case
    TAll (T.MkTVar bound) t -> do
        fr <- fresh
        let s = M.singleton (coerce bound) fr
        apply s <$> inst t
    TFun t1 t2 -> TFun <$> inst t1 <*> inst t2
    rest -> return rest

-- | Compose two substitution sets
compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (apply m1) m2 `M.union` m1

composeAll :: [Subst] -> Subst
composeAll = foldl' compose nullSubst

-- TODO: Split this class into two separate classes, one for free variables
--       and one for applying substitutions

-- | A class for substitutions
class SubstType t where
    -- | Apply a substitution to t
    apply :: Subst -> t -> t

class FreeVars t where
    -- | Get all free variables from t
    free :: t -> Set T.Ident

instance FreeVars Type where
    free :: Type -> Set T.Ident
    free (TVar (T.MkTVar a)) = S.singleton (coerce a)
    free (TAll (T.MkTVar bound) t) = S.singleton (coerce bound) `S.intersection` free t
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
            TVar (T.MkTVar a) -> case M.lookup (coerce a) sub of
                Nothing -> TVar (T.MkTVar $ coerce a)
                Just t -> t
            TAll (T.MkTVar i) t -> case M.lookup (coerce i) sub of
                Nothing -> TAll (T.MkTVar i) (apply sub t)
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

-- | Generate a new fresh variable and increment the state counter
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
                then return . TVar . T.MkTVar $ LIdent [c]
                else return . TVar . T.MkTVar . LIdent $ c : show n
  where
    next :: Char -> Char
    next 'z' = 'a'
    next a = succ a

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => T.Ident -> Type -> m a -> m a
withBinding i p = local (\st -> st{vars = M.insert i p (vars st)})

-- | Run the monadic action with several additional bindings
withBindings :: (Monad m, MonadReader Ctx m) => [(T.Ident, Type)] -> m a -> m a
withBindings xs =
    local (\st -> st{vars = foldl' (flip (uncurry M.insert)) (vars st) xs})

-- | Insert a function signature into the environment
insertSig :: T.Ident -> Maybe Type -> Infer ()
insertSig i t = modify (\st -> st{sigs = M.insert i t (sigs st)})

-- | Insert a constructor with its data type
insertInj :: T.Ident -> Type -> Infer ()
insertInj i t =
    modify (\st -> st{injections = M.insert i t (injections st)})

existInj :: T.Ident -> Infer (Maybe Type)
existInj n = gets (M.lookup n . injections)

-------- PATTERN MATCHING ---------

checkCase :: Type -> [Branch] -> Infer (Subst, [T.Branch' Type], Type)
checkCase _ [] = throwError "Atleast one case required"
checkCase expT brnchs = do
    (subs, injTs, injs, returns) <- unzip4 <$> mapM inferBranch brnchs
    let sub0 = composeAll subs
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
    let comp = sub2 `compose` sub1 `compose` sub0
    return (comp, apply comp injs, apply comp returns_type)

inferBranch :: Branch -> Infer (Subst, Type, T.Branch' Type, Type)
inferBranch (Branch pat expr) = do
    newPat@(pat, branchT) <- inferPattern pat
    (sub, newExp@(_, exprT)) <- withPattern pat (algoW expr)
    return (sub, apply sub branchT, T.Branch (apply sub newPat) (apply sub newExp), apply sub exprT)

withPattern :: T.Pattern' Type -> Infer a -> Infer a
withPattern p ma = case p of
    T.PVar (x, t) -> withBinding x t ma
    T.PInj _ ps -> foldl' (flip withPattern) ma ps
    T.PLit _ -> ma
    T.PCatch -> ma
    T.PEnum _ -> ma

inferPattern :: Pattern -> Infer (T.Pattern' Type, Type)
inferPattern = \case
    PLit lit -> let lt = litType lit in return (T.PLit (lit, lt), lt)
    PInj constr patterns -> do
        t <- gets (M.lookup (coerce constr) . injections)
        t <- maybeToRightM ("Constructor: " <> printTree constr <> " does not exist") t
        let numArgs = typeLength t - 1
        let (vs, ret) = fromJust (unsnoc $ flattenType t)
        patterns <- mapM inferPattern patterns
        unless
            (length patterns == numArgs)
            ( throwError $ Aux.do
                "The constructor"
                quote $ printTree constr
                " should have "
                show numArgs
                " arguments but has been given "
                show (length patterns)
            )
        sub <- composeAll <$> zipWithM unify vs (map snd patterns)
        return (T.PInj (coerce constr) (apply sub (map fst patterns)), apply sub ret)
    PCatch -> (T.PCatch,) <$> fresh
    PEnum p -> do
        t <- gets (M.lookup (coerce p) . injections)
        t <- maybeToRightM ("Constructor: " <> printTree p <> " does not exist") t
        unless
            (typeLength t == 1)
            ( throwError $ Aux.do
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

flattenType :: Type -> [Type]
flattenType (TFun a b) = flattenType a <> flattenType b
flattenType a = [a]

typeLength :: Type -> Int
typeLength (TFun a b) = typeLength a + typeLength b
typeLength _ = 1

litType :: Lit -> Type
litType (LInt _) = int
litType (LChar _) = char

int = TLit "Int"
char = TLit "Char"

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
    catchError ma (\x -> throwError $ x <> " in expression: \n" <> printTree exp)

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

type Error = String
type Subst = Map T.Ident Type

newtype Infer a = Infer {runInfer :: StateT Env (ReaderT Ctx (ExceptT Error Identity)) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadError Error, MonadState Env)

quote :: String -> String
quote s = "'" ++ s ++ "'"
