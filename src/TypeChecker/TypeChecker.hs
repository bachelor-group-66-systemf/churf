{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for type checking and inference using algorithm W, Hindley-Milner
module TypeChecker.TypeChecker where

import Auxiliary
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Data.List.Extra (unsnoc)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
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
    _ <- checkDef bs
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
    e@(_, args_t) <- inferExp lambda
    s <- gets sigs
    case M.lookup (coerce name) s of
        Just (Just t') -> do
            unless
                (args_t `typeEq` t')
                ( throwError $
                    "Inferred type '"
                        ++ printTree args_t
                        ++ " does not match specified type '"
                        ++ printTree t'
                        ++ "'"
                )
            return $ T.Bind (coerce name, t') [] e
        _ -> do
            insertSig (coerce name) (Just args_t)
            return (T.Bind (coerce name, args_t) [] e)

typeEq :: T.Type -> T.Type -> Bool
typeEq (T.TFun l r) (T.TFun l' r') = typeEq l l' && typeEq r r'
typeEq (T.TLit a) (T.TLit b) = a == b
typeEq (T.TData name a) (T.TData name' b) =
    length a == length b
        && name == name'
        && and (zipWith typeEq a b)
typeEq (T.TAll _ t1) t2 = t1 `typeEq` t2
typeEq t1 (T.TAll _ t2) = t1 `typeEq` t2
typeEq (T.TVar _) (T.TVar _) = True
typeEq _ _ = False

isMoreSpecificOrEq :: T.Type -> T.Type -> Bool
isMoreSpecificOrEq t1 (T.TAll _ t2) = isMoreSpecificOrEq t1 t2
isMoreSpecificOrEq (T.TFun a b) (T.TFun c d) =
    isMoreSpecificOrEq a c && isMoreSpecificOrEq b d
isMoreSpecificOrEq (T.TData n1 ts1) (T.TData n2 ts2) =
    n1 == n2
        && length ts1 == length ts2
        && and (zipWith isMoreSpecificOrEq ts1 ts2)
isMoreSpecificOrEq _ (T.TVar _) = True
isMoreSpecificOrEq a b = a == b

isPoly :: Type -> Bool
isPoly (TAll _ _) = True
isPoly (TVar _) = True
isPoly _ = False

inferExp :: Exp -> Infer T.ExpT
inferExp e = do
    (s, (e', t)) <- algoW e
    let subbed = apply s t
    return $ second (const subbed) (e', t)

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
        name _ = error "Bug: Data types should not be able to be typed over non type variables"

instance NewType Constructor T.Constructor where
    toNew (Constructor name xs) = T.Constructor (coerce name) (toNew xs)

instance NewType TVar T.TVar where
    toNew (MkTVar i) = T.MkTVar $ coerce i

instance NewType a b => NewType [a] [b] where
    toNew = map toNew

algoW :: Exp -> Infer (Subst, T.ExpT)
algoW = \case
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
                    Just Nothing -> do
                        fr <- fresh
                        insertSig (coerce i) (Just fr)
                        return (nullSubst, (T.EId $ coerce i, fr))
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
        (s0, (e0', t0)) <- algoW e0
        applySt s0 $ do
            (s1, (e1', t1)) <- algoW e1
            s2 <- exprErr (unify (apply s1 t0) (T.TFun t1 fr)) err
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
        let t' = apply comp ret_t
        return (comp, apply comp (T.ECase (e', t) injs, t'))

makeLambda :: Exp -> [T.Ident] -> Exp
makeLambda = foldl (flip (EAbs . coerce))

-- | Unify two types producing a new substitution
unify :: T.Type -> T.Type -> Infer Subst
unify t0 t1 = do
    case (t0, t1) of
        (T.TFun a b, T.TFun c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s1 `compose` s2
        ----------- TODO: CAREFUL!!!! THIS IS PROBABLY WRONG!!! -----------
        (T.TVar (T.MkTVar a), t@(T.TData _ _)) -> return $ M.singleton a t
        (t@(T.TData _ _), T.TVar (T.MkTVar b)) -> return $ M.singleton b t
        -------------------------------------------------------------------
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

composeAll :: [Subst] -> Subst
composeAll = foldl' compose nullSubst

-- TODO: Split this class into two separate classes, one for free variables
--       and one for applying substitutions

-- | A class representing free variables functions
class SubstType t where
    -- | Apply a substitution to t
    apply :: Subst -> t -> t

class FreeVars t where
    -- | Get all free variables from t
    free :: t -> Set T.Ident

instance FreeVars T.Type where
    free :: T.Type -> Set T.Ident
    free (T.TVar (T.MkTVar a)) = S.singleton a
    free (T.TAll (T.MkTVar bound) t) = S.singleton bound `S.intersection` free t
    free (T.TLit _) = mempty
    free (T.TFun a b) = free a `S.union` free b
    -- \| Not guaranteed to be correct
    free (T.TData _ a) =
        foldl' (\acc x -> free x `S.union` acc) S.empty a

instance SubstType T.Type where
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

instance SubstType (Map T.Ident T.Type) where
    apply :: Subst -> Map T.Ident T.Type -> Map T.Ident T.Type
    apply s = M.map (apply s)

instance SubstType T.Exp where
    apply :: Subst -> T.Exp -> T.Exp
    apply s = \case
        T.EId i -> T.EId i
        T.ELit lit -> T.ELit lit
        T.ELet (T.Bind (ident, t1) args e1) e2 ->
            T.ELet
                (T.Bind (ident, apply s t1) args (apply s e1))
                (apply s e2)
        T.EApp e1 e2 -> T.EApp (apply s e1) (apply s e2)
        T.EAdd e1 e2 -> T.EAdd (apply s e1) (apply s e2)
        T.EAbs ident e -> T.EAbs ident (apply s e)
        T.ECase e brnch -> T.ECase (apply s e) (apply s brnch)

instance SubstType T.Branch where
    apply :: Subst -> T.Branch -> T.Branch
    apply s (T.Branch (i, t) e) = T.Branch (apply s i, apply s t) (apply s e)

instance SubstType T.Pattern where
    apply :: Subst -> T.Pattern -> T.Pattern
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

instance SubstType T.Id where
    apply s (name, t) = (name, apply s t)

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

inferBranch :: Branch -> Infer (Subst, T.Type, T.Branch, T.Type)
inferBranch (Branch pat expr) = do
    newPat@(pat, branchT) <- inferPattern pat
    (sub, newExp@(_, exprT)) <- withPattern pat (algoW expr)
    return (sub, branchT, T.Branch (apply sub newPat) (apply sub newExp), apply sub exprT)

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
        let numArgs = typeLength t - 1
        let (vs, ret) = fromJust (unsnoc $ flattenType t)
        patterns <- mapM inferPattern patterns
        unless (length patterns == numArgs) (throwError $ "The constructor '" ++ printTree constr ++ "'" ++ " should have " ++ show numArgs ++ " arguments but has been given " ++ show (length patterns))
        sub <- composeAll <$> zipWithM unify vs (map snd patterns)
        return (T.PInj (coerce constr) (map fst patterns), apply sub ret)
    PCatch -> (T.PCatch,) <$> fresh
    PEnum p -> do
        t <- gets (M.lookup (coerce p) . constructors)
        t <- maybeToRightM ("Constructor: " <> printTree p <> " does not exist") t
        unless (typeLength t == 1) (throwError $ "The constructor '" ++ printTree p ++ "'" ++ " should have " ++ show (typeLength t - 1) ++ " arguments but has been given 0")
        return (T.PEnum $ coerce p, t)
    PVar x -> do
        fr <- fresh
        let pvar = T.PVar (coerce x, fr)
        return (pvar, fr)

flattenType :: T.Type -> [T.Type]
flattenType (T.TFun a b) = flattenType a <> flattenType b
flattenType a = [a]

typeLength :: T.Type -> Int
typeLength (T.TFun a b) = typeLength a + typeLength b
typeLength _ = 1

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
    catchError ma (\x -> throwError $ x <> " in expression: \n" <> printTree exp)

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = foldl' (\(as, bs, cs, ds) (a, b, c, d) -> (as ++ [a], bs ++ [b], cs ++ [c], ds ++ [d])) ([], [], [], [])
