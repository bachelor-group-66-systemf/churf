{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for type checking and inference using algorithm W, Hindley-Milner
module TypeChecker.TypeCheckerHm where

import           Auxiliary
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor            (second)
import           Data.Coerce               (coerce)
import           Data.Foldable             (traverse_)
import           Data.Functor.Identity     (Identity, runIdentity)
import           Data.List                 (foldl')
import           Data.List.Extra           (unsnoc)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Debug.Trace               (trace)
import           Grammar.Abs
import           Grammar.Print             (printTree)
import qualified TypeChecker.TypeCheckerIr as T
import           TypeChecker.TypeCheckerIr (Ident (..))


newtype Ctx = Ctx {vars :: Map Ident Type}
    deriving (Show)

data Env = Env
    { count        :: Int
    , sigs         :: Map Ident (Maybe Type)
    , constructors :: Map Ident Type
    }
    deriving (Show)

type Error = String
type Subst = Map Ident Type

type Infer = StateT Env (ReaderT Ctx (ExceptT Error Identity))

initCtx = Ctx mempty

initEnv = Env 0 mempty mempty

runPretty :: Exp -> Either Error String
runPretty = fmap (printTree . fst) . run . inferExp

run :: Infer a -> Either Error a
run = runC initEnv initCtx

runC :: Env -> Ctx -> Infer a -> Either Error a
runC e c = runIdentity . runExceptT . flip runReaderT c . flip evalStateT e

typecheck :: Program -> Either Error (T.Program' Type)
typecheck = run . checkPrg

checkData :: Data -> Infer ()
checkData d = do
    case d of
        (Data typ@(TData name ts) constrs) -> do
            unless
                (all isPoly ts)
                (throwError $ unwords ["Data type incorrectly declared"])
            traverse_
                ( \(Inj name' t') ->
                    if typ == retType t'
                        then insertConstr (coerce name') t'
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
retType a           = a

checkPrg :: Program -> Infer (T.Program' Type)
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
            insertSig (coerce n) (Just t) >> preRun xs
        DBind (Bind n _ _) -> do
            s <- gets sigs
            case M.lookup (coerce n) s of
                Nothing -> insertSig (coerce n) Nothing >> preRun xs
                Just _  -> preRun xs
        DData d@(Data _ _) -> checkData d >> preRun xs

    checkDef :: [Def] -> Infer [T.Def' Type]
    checkDef [] = return []
    checkDef (x : xs) = case x of
        (DBind b) -> do
            b' <- checkBind b
            fmap (T.DBind b' :) (checkDef xs)
        (DData d) -> do
          fmap (T.DData (coerceData d) :) (checkDef xs)
        (DSig _) -> checkDef xs

    coerceData (Data t injs) = T.Data t $ map (\(Inj name typ) -> T.Inj (coerce name) typ) injs

checkBind :: Bind -> Infer (T.Bind' Type)
checkBind err@(Bind name args e) = do
    let lambda = makeLambda e (reverse (coerce args))
    (_, lambdaT) <- inferExp lambda
    args <- zip args <$> mapM (const fresh) args
    withBindings (map coerce args) $ do
        e@(_, _) <- inferExp e
        s <- gets sigs
        case M.lookup (coerce name) s of
            Just (Just t) -> do
                sub <- bindErr (unify t lambdaT) err
                let newT = apply sub t
                insertSig (coerce name) (Just newT)
                return $ T.Bind (apply sub (coerce name, newT)) (map coerce args) e
            _ -> do
                insertSig (coerce name) (Just lambdaT)
                return (T.Bind (coerce name, lambdaT) (map coerce args) e)

isMoreSpecificOrEq :: Type -> Type -> Bool
isMoreSpecificOrEq _ (TAll _ _) = True
isMoreSpecificOrEq (TFun a b) (TFun c d) =
    isMoreSpecificOrEq a c && isMoreSpecificOrEq b d
isMoreSpecificOrEq (TData n1 ts1) (TData n2 ts2) =
    n1 == n2
        && length ts1 == length ts2
        && and (zipWith isMoreSpecificOrEq ts1 ts2)
isMoreSpecificOrEq a b = a == b

isPoly :: Type -> Bool
isPoly (TAll _ _) = True
isPoly (TVar _)   = True
isPoly _          = False

inferExp :: Exp -> Infer (T.ExpT' Type)
inferExp e = do
    (s, (e', t)) <- algoW e
    let subbed = apply s t
    return $ replace subbed (e', t)

replace :: Type -> T.ExpT' Type -> T.ExpT' Type
replace t = second (const t)


algoW :: Exp -> Infer (Subst, T.ExpT' Type)
algoW = \case
    -- \| TODO: More testing need to be done. Unsure of the correctness of this
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
        applySt s1 $ do
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
                    Just Nothing ->
                        (\x -> (nullSubst, (T.EVar $ coerce i, x))) <$> fresh
                    Nothing -> throwError $ "Unbound variable: " <> printTree i
    EInj i -> do
        constr <- gets constructors
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
        let t' = apply comp ret_t
        return (comp, apply comp (T.ECase (e', t) injs, t'))

makeLambda :: Exp -> [Ident] -> Exp
makeLambda = foldl (flip (EAbs . coerce))

-- | Unify two types producing a new substitution
unify :: Type -> Type -> Infer Subst
unify t0 t1 = do
    case (t0, t1) of
        (TFun a b, TFun c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s1 `compose` s2
        -- TODO: BEWARY. THIS IS PROBABLY WRONG!!!
        (TVar (MkTVar a), t@(TData _ _)) -> return $ M.singleton (coerce a) t
        (t@(TData _ _), TVar (MkTVar b)) -> return $ M.singleton (coerce b) t
        (TVar (MkTVar a), t) -> occurs (coerce a) t
        (t, TVar (MkTVar b)) -> occurs (coerce b) t
        (TAll _ t, b) -> unify t b
        (a, TAll _ t) -> unify a t
        (TLit a, TLit b) ->
            if a == b
                then return M.empty
                else
                    throwError
                        . unwords
                        $ [ "Can not unify"
                          , "'" <> printTree (TLit a) <> "'"
                          , "with"
                          , "'" <> printTree (TLit b) <> "'"
                          ]
        (TData name t, TData name' t') ->
            if name == name' && length t == length t'
                then do
                    xs <- zipWithM unify t t'
                    return $ foldr compose nullSubst xs
                else
                    throwError $
                        unwords
                            [ "Type constructor:"
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
occurs :: Ident -> Type -> Infer Subst
occurs i t@(TVar _) = return (M.singleton i t)
occurs i t =
    if S.member i (free t)
        then
            throwError $
                unwords
                    [ "Occurs check failed, can't unify"
                    , printTree (TVar . MkTVar $ coerce i)
                    , "with"
                    , printTree t
                    ]
        else return $ M.singleton i t

-- | Generalize a type over all free variables in the substitution set
generalize :: Map Ident Type -> Type -> Type
generalize env t = go freeVars $ removeForalls t
  where
    freeVars :: [Ident]
    freeVars = S.toList $ free t S.\\ free env
    go :: [Ident] -> Type -> Type
    go [] t       = t
    go (x : xs) t = TAll (MkTVar $ coerce x) (go xs t)
    removeForalls :: Type -> Type
    removeForalls (TAll _ t)   = removeForalls t
    removeForalls (TFun t1 t2) = TFun (removeForalls t1) (removeForalls t2)
    removeForalls t            = t

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

-- | Compose two substitution sets
compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (apply m1) m2 `M.union` m1

-- TODO: Split this class into two separate classes, one for free variables
--       and one for applying substitutions

-- | A class representing free variables functions
class SubstType t where
    -- | Apply a substitution to t
    apply :: Subst -> t -> t

class FreeVars t where
    -- | Get all free variables from t
    free :: t -> Set Ident

instance FreeVars Type where
    free :: Type -> Set Ident
    free (TVar (MkTVar a)) = S.singleton (coerce a)
    free (TAll (MkTVar bound) t) = S.singleton (coerce bound) `S.intersection` free t
    free (TLit _) = mempty
    free (TFun a b) = free a `S.union` free b
    -- \| Not guaranteed to be correct
    free (TData _ a) =
        foldl' (\acc x -> free x `S.union` acc) S.empty a

instance SubstType Type where
    apply :: Subst -> Type -> Type
    apply sub t = do
        case t of
            TLit a -> TLit a
            TVar (MkTVar a) -> case M.lookup (coerce a) sub of
                Nothing -> TVar (MkTVar $ coerce a)
                Just t  -> t
            TAll (MkTVar i) t -> case M.lookup (coerce i) sub of
                Nothing -> TAll (MkTVar i) (apply sub t)
                Just _  -> apply sub t
            TFun a b -> TFun (apply sub a) (apply sub b)
            TData name a -> TData name (map (apply sub) a)
            _ -> error "TEVar"
instance FreeVars (Map Ident Type) where
    free :: Map Ident Type -> Set Ident
    free m = foldl' S.union S.empty (map free $ M.elems m)

instance SubstType (Map Ident Type) where
    apply :: Subst -> Map Ident Type -> Map Ident Type
    apply s = M.map (apply s)

instance SubstType (T.ExpT' Type) where
    apply :: Subst -> T.ExpT' Type -> T.ExpT' Type
    apply s = \case
        (T.EVar i, outerT) -> (T.EVar i, apply s outerT)
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
        (T.ECase e brnch, t) -> (T.ECase (apply s e) (apply s brnch), apply s t)

instance SubstType (T.Branch' Type) where
    apply :: Subst -> T.Branch' Type -> T.Branch' Type
    apply s (T.Branch (i, t) e) = T.Branch (apply s i, apply s t) (apply s e)

instance SubstType (T.Pattern' Type) where
    apply :: Subst -> T.Pattern' Type -> T.Pattern' Type
    apply s = \case
        T.PVar (iden, t) -> T.PVar (iden, apply s t)
        T.PLit (lit, t)  -> T.PLit (lit, apply s t)
        T.PInj i ps      -> T.PInj i $ apply s ps
        T.PCatch         -> T.PCatch
        T.PEnum i        -> T.PEnum i

instance SubstType a => SubstType [a] where
    apply s = map (apply s)

instance SubstType (T.Id' Type) where
    apply s (name, t) = (name, apply s t)

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
    return . TVar . T.MkTVar . LIdent $ show n

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => Ident -> Type -> m a -> m a
withBinding i p = local (\st -> st{vars = M.insert i p (vars st)})

-- | Run the monadic action with several additional bindings
withBindings :: (Monad m, MonadReader Ctx m) => [(Ident, Type)] -> m a -> m a
withBindings xs =
    local (\st -> st{vars = foldl' (flip (uncurry M.insert)) (vars st) xs})

-- | Insert a function signature into the environment
insertSig :: Ident -> Maybe Type -> Infer ()
insertSig i t = modify (\st -> st{sigs = M.insert i t (sigs st)})

-- | Insert a constructor with its data type
insertConstr :: Ident -> Type -> Infer ()
insertConstr i t =
    modify (\st -> st{constructors = M.insert i t (constructors st)})

-------- PATTERN MATCHING ---------

checkCase :: Type -> [Branch] -> Infer (Subst, [T.Branch' Type], Type)
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
inferBranch :: Branch -> Infer (Type, T.Branch' Type, Type)
inferBranch (Branch pat expr) = do
    newPat@(pat, branchT) <- inferPattern pat
    newExp@(_, exprT) <- withPattern pat (inferExp expr)
    return (branchT, T.Branch newPat newExp, exprT)

withPattern :: T.Pattern' Type -> Infer a -> Infer a
withPattern p ma = case p of
    T.PVar (x, t) -> withBinding x t ma
    T.PInj _ ps   -> foldl' (flip withPattern) ma ps
    T.PLit _      -> ma
    T.PCatch      -> ma
    T.PEnum _     -> ma

inferPattern :: Pattern -> Infer (T.Pattern' Type, Type)
inferPattern = \case
    PLit lit -> let lt = litType lit in return (T.PLit (lit, lt), lt)
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

flattenType :: Type -> [Type]
flattenType (TFun a b) = flattenType a <> flattenType b
flattenType a          = [a]

litType :: Lit -> Type
litType (LInt _)  = int
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
        TFun t1 t2   -> go (acc <> [t1]) (i - 1) t2
        _            -> error "Number of parameters and type doesn't match"

exprErr :: Infer a -> Exp -> Infer a
exprErr ma exp =
    catchError ma (\x -> throwError $ x <> " on expression: " <> printTree exp)

bindErr :: Infer a -> Bind -> Infer a
bindErr ma exp =
    catchError ma (\x -> throwError $ x <> " on expression: " <> printTree exp)
