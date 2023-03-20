{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module for type checking and inference using algorithm W, Hindley-Milner
module TypeChecker.TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import Data.List (foldl')
import Data.List.Extra (allSame)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tree (flatten)
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

{- | Start by freshening the type variable of data types to avoid clash with
other user defined polymorphic types
-}
freshenData :: Data -> Infer Data
freshenData (Data (Constr name ts) constrs) = do
    let xs = (S.toList . free) =<< ts
    frs <- traverse (const fresh) xs
    let m = M.fromList $ zip xs frs
    return $ Data (Constr name (map (freshenType m) ts)) (map (\(Constructor ident t) -> Constructor ident (freshenType m t)) constrs)

{- | Freshen all polymorphic variables, regardless of name
| freshenType "d" (a -> b -> c) becomes (d -> d -> d)
-}
freshenType :: Map Ident Type -> Type -> Type
freshenType m t = case t of
    TPol poly -> fromMaybe (error "bug in \'free\'") (M.lookup poly m)
    TMono mono -> TMono mono
    TArr t1 t2 -> TArr (freshenType m t1) (freshenType m t2)
    TConstr (Constr ident ts) -> TConstr (Constr ident (map (freshenType m) ts))

checkData :: Data -> Infer ()
checkData d = do
    d' <- freshenData d
    case d' of
        (Data typ@(Constr name ts) constrs) -> do
            unless
                (all isPoly ts)
                (throwError $ unwords ["Data type incorrectly declared"])
            traverse_
                ( \(Constructor name' t') ->
                    if TConstr typ == retType t'
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
retType (TArr _ t2) = retType t2
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
        DBind (Bind n t _ _ _) -> insertSig n t >> preRun xs
        DData d@(Data _ _) -> checkData d >> preRun xs

    checkDef :: [Def] -> Infer [T.Def]
    checkDef [] = return []
    checkDef (x : xs) = case x of
        (DBind b) -> do
            b' <- checkBind b
            fmap (T.DBind b' :) (checkDef xs)
        (DData d) -> do
            d' <- freshenData d
            fmap (T.DData d' :) (checkDef xs)

-- TODO: Unify top level types with the types of the expressions beneath
-- PERHAPS DONE
checkBind :: Bind -> Infer T.Bind
checkBind (Bind n t _ args e) = do
    (t', e) <- inferExp $ makeLambda e (reverse args)
    s <- unify t' t
    let t'' = apply s t
    unless
        (t `typeEq` t'')
        ( throwError $
            unwords
                [ "Top level signature"
                , printTree t
                , "does not match body with inferred type:"
                , printTree t''
                ]
        )
    return $ T.Bind (n, t) (apply s e)
  where
    makeLambda :: Exp -> [Ident] -> Exp
    makeLambda = foldl (flip EAbs)

{- | Check if two types are considered equal
  For the purpose of the algorithm two polymorphic types are always considered
  equal
-}
typeEq :: Type -> Type -> Bool
typeEq (TArr l r) (TArr l' r') = typeEq l l' && typeEq r r'
typeEq (TMono a) (TMono b) = a == b
typeEq (TConstr (Constr name a)) (TConstr (Constr name' b)) =
    length a == length b
        && name == name'
        && and (zipWith typeEq a b)
typeEq (TPol _) (TPol _) = True
typeEq _ _ = False

isMoreSpecificOrEq :: Type -> Type -> Bool
isMoreSpecificOrEq _ (TPol _) = True
isMoreSpecificOrEq (TArr a b) (TArr c d) =
    isMoreSpecificOrEq a c && isMoreSpecificOrEq b d
isMoreSpecificOrEq (TConstr (Constr n1 ts1)) (TConstr (Constr n2 ts2)) =
    n1 == n2
        && length ts1 == length ts2
        && and (zipWith isMoreSpecificOrEq ts1 ts2)
isMoreSpecificOrEq a b = a == b

isPoly :: Type -> Bool
isPoly (TPol _) = True
isPoly _ = False

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
    T.ELet (T.Bind (n, _) e1) e2 -> T.ELet (T.Bind (n, t) e1) e2
    T.ECase _ expr injs -> T.ECase t expr injs

algoW :: Exp -> Infer (Subst, Type, T.Exp)
algoW = \case
    -- \| TODO: More testing need to be done. Unsure of the correctness of this
    EAnn e t -> do
        (s1, t', e') <- algoW e
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
            s2 <- unify t t'
            let composition = s2 `compose` s1
            return (composition, t, apply composition e')

    -- \| ------------------
    -- \|   Γ ⊢ i : Int, ∅

    ELit lit ->
        let lt = litType lit
         in return (nullSubst, lt, T.ELit lt lit)
    -- \| x : σ ∈ Γ   τ = inst(σ)
    -- \| ----------------------
    -- \|     Γ ⊢ x : τ, ∅

    EId i -> do
        var <- asks vars
        case M.lookup i var of
            Just t -> inst t >>= \x -> return (nullSubst, x, T.EId (i, x))
            Nothing -> do
                sig <- gets sigs
                case M.lookup i sig of
                    Just t -> return (nullSubst, t, T.EId (i, t))
                    Nothing -> do
                        constr <- gets constructors
                        case M.lookup i constr of
                            Just t -> return (nullSubst, t, T.EId (i, t))
                            Nothing ->
                                throwError $
                                    "Unbound variable: " ++ show i

    -- \| τ = newvar   Γ, x : τ ⊢ e : τ', S
    -- \| ---------------------------------
    -- \|     Γ ⊢ w λx. e : Sτ → τ', S

    EAbs name e -> do
        fr <- fresh
        withBinding name (Forall [] fr) $ do
            (s1, t', e') <- algoW e
            let varType = apply s1 fr
            let newArr = TArr varType t'
            return (s1, newArr, apply s1 $ T.EAbs newArr (name, varType) e')

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
            s3 <- unify (apply s2 t0) (TMono "Int")
            s4 <- unify (apply s3 t1) (TMono "Int")
            let composition = s4 `compose` s3 `compose` s2 `compose` s1
            return
                ( composition
                , TMono "Int"
                , apply composition $ T.EAdd (TMono "Int") e0' e1'
                )

    -- \| Γ ⊢ e₀ : τ₀, S₀    S₀Γ ⊢ e₁ : τ₁, S1
    -- \| τ' = newvar    S₂ = mgu(S₁τ₀, τ₁ → τ')
    -- \| --------------------------------------
    -- \|       Γ ⊢ e₀ e₁ : S₂τ', S₂S₁S₀

    EApp e0 e1 -> do
        fr <- fresh
        (s0, t0, e0') <- algoW e0
        applySt s0 $ do
            (s1, t1, e1') <- algoW e1
            -- applySt s1 $ do
            s2 <- unify (apply s1 t0) (TArr t1 fr)
            let t = apply s2 fr
            let composition = s2 `compose` s1 `compose` s0
            return (composition, t, apply composition $ T.EApp t e0' e1')

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
            let composition = s2 `compose` s1
            return (composition, t2, apply composition $ T.ELet (T.Bind (name, t2) e0') e1')

    -- TODO: give caseExpr a concrete type before proceeding
    -- probably by returning substitutions in the functions used in this body
    ECase caseExpr injs -> do
        (sub, _, e') <- algoW caseExpr
        trace ("SUB: " ++ show sub) return ()
        t <- checkCase caseExpr injs
        return (sub, t, T.ECase t e' (map (\(Inj i _) -> T.Inj (i, t) e') injs))

-- | Unify two types producing a new substitution
unify :: Type -> Type -> Infer Subst
unify t0 t1 = do
    case (t0, t1) of
        (TArr a b, TArr c d) -> do
            s1 <- unify a c
            s2 <- unify (apply s1 b) (apply s1 d)
            return $ s1 `compose` s2
        (TPol a, b) -> occurs a b
        (a, TPol b) -> occurs b a
        (TMono a, TMono b) ->
            if a == b then return M.empty else throwError "Types do not unify"
        (TConstr (Constr name t), TConstr (Constr name' t')) ->
            if name == name' && length t == length t'
                then do
                    xs <- zipWithM unify t t'
                    return $ foldr compose nullSubst xs
                else
                    throwError $
                        unwords
                            [ "Type constructor:"
                            , printTree name
                            , "(" ++ printTree t ++ ")"
                            , "does not match with:"
                            , printTree name'
                            , "(" ++ printTree t' ++ ")"
                            ]
        (a, b) ->
            throwError . unwords $
                [ "Type:"
                , printTree a
                , "can't be unified with:"
                , printTree b
                ]

{- | Check if a type is contained in another type.
I.E. { a = a -> b } is an unsolvable constraint since there is no substitution
where these are equal
-}
occurs :: Ident -> Type -> Infer Subst
occurs i t@(TPol a) = return (M.singleton i t)
occurs i t =
    if S.member i (free t)
        then
            throwError $
                unwords
                    [ "Occurs check failed, can't unify"
                    , printTree (TPol i)
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
    free (TPol a) = S.singleton a
    free (TMono _) = mempty
    free (TArr a b) = free a `S.union` free b
    -- \| Not guaranteed to be correct
    free (TConstr (Constr _ a)) =
        foldl' (\acc x -> free x `S.union` acc) S.empty a

    apply :: Subst -> Type -> Type
    apply sub t = do
        case t of
            TMono a -> TMono a
            TPol a -> case M.lookup a sub of
                Nothing -> TPol a
                Just t -> t
            TArr a b -> TArr (apply sub a) (apply sub b)
            TConstr (Constr name a) -> TConstr (Constr name (map (apply sub) a))

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

instance FreeVars T.Exp where
    free :: T.Exp -> Set Ident
    free = error "free not implemented for T.Exp"
    apply :: Subst -> T.Exp -> T.Exp
    apply s = \case
        T.EId (ident, t) -> T.EId (ident, apply s t)
        T.ELit t lit -> T.ELit (apply s t) lit
        T.ELet (T.Bind (ident, t) e1) e2 -> T.ELet (T.Bind (ident, apply s t) (apply s e1)) (apply s e2)
        T.EApp t e1 e2 -> T.EApp (apply s t) (apply s e1) (apply s e2)
        T.EAdd t e1 e2 -> T.EAdd (apply s t) (apply s e1) (apply s e2)
        T.EAbs t1 (ident, t2) e -> T.EAbs (apply s t1) (ident, apply s t2) (apply s e)
        T.ECase t e injs -> T.ECase (apply s t) (apply s e) (apply s injs)

instance FreeVars T.Inj where
    free :: T.Inj -> Set Ident
    free = undefined
    apply :: Subst -> T.Inj -> T.Inj
    apply s (T.Inj (i, t) e) = T.Inj (i, apply s t) (apply s e)

instance FreeVars [T.Inj] where
    free :: [T.Inj] -> Set Ident
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
    return . TPol . Ident $ show n

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => Ident -> Poly -> m a -> m a
withBinding i p = local (\st -> st{vars = M.insert i p (vars st)})

-- | Insert a function signature into the environment
insertSig :: Ident -> Type -> Infer ()
insertSig i t = modify (\st -> st{sigs = M.insert i t (sigs st)})

-- | Insert a constructor with its data type
insertConstr :: Ident -> Type -> Infer ()
insertConstr i t =
    modify (\st -> st{constructors = M.insert i t (constructors st)})

-------- PATTERN MATCHING ---------

unifyAll :: [Type] -> Infer [Subst]
unifyAll [] = return []
unifyAll [_] = return []
unifyAll (x : y : xs) = do
    uni <- unify x y
    all <- unifyAll (y : xs)
    return $ uni : all

checkCase :: Exp -> [Inj] -> Infer Type
checkCase e injs = do
    expT <- fst <$> inferExp e
    (injTs, returns) <- mapAndUnzipM checkInj injs
    unifyAll (expT : injTs)
    subst <- foldl1 compose <$> zipWithM unify returns (tail returns)
    let substed = map (apply subst) returns
    unless (allSame substed || null substed) (throwError "Different return types of case, or no cases")
    return $ head substed

{- | fst = type of init
| snd = type of expr
-}
checkInj :: Inj -> Infer (Type, Type)
checkInj (Inj it expr) = do
    initT <- inferInit it
    (exprT, _) <- inferExp expr
    return (initT, exprT)

inferInit :: Init -> Infer Type
inferInit = \case
    InitLit lit -> return $ litType lit
    InitConstr fn vars -> do
        gets (M.lookup fn . constructors) >>= \case
            Nothing -> throwError $ "Constructor: " ++ printTree fn ++ " does not exist"
            Just a -> do
                let ft = init $ flattenType a
                case compare (length vars) (length ft) of
                    EQ -> return . last $ flattenType a
                    _ -> throwError "Partial pattern match not allowed"
    InitCatch -> fresh

flattenType :: Type -> [Type]
flattenType (TArr a b) = flattenType a ++ flattenType b
flattenType a = [a]

litType :: Literal -> Type
litType (LInt _) = TMono "Int"
