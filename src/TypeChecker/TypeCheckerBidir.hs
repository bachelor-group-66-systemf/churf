{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeChecker.TypeCheckerBidir (typecheck) where

import           Auxiliary                 (int, maybeToRightM, onM, snoc,
                                            typeof)
import           Control.Applicative       (Applicative (liftA2), (<|>))
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            forM, runExceptT, unless, zipWithM,
                                            zipWithM_)
import           Control.Monad.Extra       (fromMaybeM, ifM)
import           Control.Monad.State       (MonadState, State, evalState, gets,
                                            modify)
import           Data.Coerce               (coerce)
import           Data.Foldable             (foldlM)
import           Data.Function             (on)
import           Data.List                 (intercalate)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isNothing)
import           Data.Sequence             (Seq (..))
import qualified Data.Sequence             as S
import qualified Data.Set                  as Set
import           Data.Tuple.Extra          (second)
import           Debug.Trace               (trace)
import           Grammar.Abs
import           Grammar.ErrM
import           Grammar.Print             (printTree)
import           Prelude                   hiding (exp)
import qualified TypeChecker.TypeCheckerIr as T
import           TypeChecker.TypeCheckerIr (T, T')

-- Implementation is derived from the paper (Dunfield and Krishnaswami 2013)
-- https://doi.org/10.1145/2500365.2500582
--
-- TODO
-- • Fix problems with types in Pattern/Branch in TypeCheckerIr
-- • Remove EAdd
-- • Add kinds!!

data EnvElem = EnvVar         LIdent Type -- ^ Term variable typing. x : A
             | EnvTVar        TVar        -- ^ Universal type variable. α
             | EnvTEVar       TEVar       -- ^ Existential unsolved type variable. ά
             | EnvTEVarSolved TEVar Type  -- ^ Existential solved type variable. ά = τ
             | EnvMark        TEVar       -- ^ Scoping Marker. ▶ ά
               deriving (Eq, Show)

type Env = Seq EnvElem

-- | Ordered context
-- Γ ::= ・| Γ, α | Γ, ά | Γ, ▶ ά | Γ, x:A
data Cxt = Cxt
    { env         :: Env             -- ^ Local scope context  Γ
    , sig         :: Map LIdent Type -- ^ Top-level signatures x : A
    , binds       :: Map LIdent Exp  -- ^ Top-level binds x : e
    , next_tevar  :: Int             -- ^ Counter to distinguish ά
    , data_injs   :: Map UIdent Type -- ^ Data injections (constructors) K/inj : A
    , currentBind :: LIdent          -- ^ Used for recursive functions
    } deriving (Show, Eq)

newtype Tc a = Tc { runTc :: ExceptT String (State Cxt) a }
    deriving (Functor, Applicative, Monad, MonadState Cxt, MonadError String)


initCxt :: [Def] -> Cxt
initCxt defs = Cxt
            { env        = mempty
            , sig        = Map.fromList [ (name, t)
                                        | DSig' name t <- defs
                                        ]
            , binds      = Map.fromList [ (name, foldr EAbs rhs vars)
                                        | DBind' name vars rhs <- defs
                                        ]
            , next_tevar = 0
            , data_injs  = Map.fromList [ (name, foldr TAll t $ unboundedTVars t)
                                        | DData (Data _ injs) <- defs
                                        , Inj name t <- injs
                                        ]
            , currentBind = ""
            }
  where
    unboundedTVars = uncurry (Set.\\)  . go (mempty, mempty)
      where
        go (unbounded, bounded) = \case
          TAll tvar t  -> go (unbounded, Set.insert tvar bounded) t
          TVar tvar    -> (Set.insert tvar unbounded, bounded)
          TFun t1 t2   -> foldl go (unbounded, bounded) [t1, t2]
          TData _ typs -> foldl go (unbounded, bounded) typs
          _            -> (unbounded, bounded)

typecheck :: Program -> Err (T.Program' Type)
typecheck (Program defs) = do
    dataTypes' <- mapM typecheckDataType [ d | DData d <- defs ]
    binds' <- typecheckBinds (initCxt defs) [b | DBind b <- defs]
    pure . T.Program $ map T.DData dataTypes' ++ map T.DBind binds'

typecheckBinds :: Cxt -> [Bind] -> Err [T.Bind' Type]
typecheckBinds cxt = flip evalState cxt
                   . runExceptT
                   . runTc
                   . mapM typecheckBind

typecheckBind :: Bind -> Tc (T.Bind' Type)
typecheckBind (Bind name vars rhs) = do
    modify $ \cxt -> cxt { currentBind = name }
    bind'@(T.Bind (name, typ) _ _) <- lookupSig name >>= \case
        Just t  -> do
            (rhs', _) <- check (foldr EAbs rhs vars) t
            pure (T.Bind (coerce name, t) [] (rhs', t))
        Nothing -> do
            (e, t) <- apply =<< infer (foldr EAbs rhs vars)
            pure (T.Bind (coerce name, t) [] (e, t))
    env <- gets env
    unless (isComplete env) err
    insertSig (coerce name) typ
    putEnv Empty
    pure bind'
  where
    err = throwError $ unlines
        [ "Type inference failed: " ++ printTree (Bind name vars rhs)
        , "Did you forget to add type annotation to a polymorphic function?"
        ]

-- TODO remove some checks
typecheckDataType :: Data -> Err (T.Data' Type)
typecheckDataType (Data typ injs) = do
    (name, tvars) <- go [] typ
    injs' <- mapM (\i -> typecheckInj i name tvars) injs
    pure (T.Data typ injs')
  where
    go tvars = \case
      TAll tvar t -> go (tvar:tvars) t
      TData name typs
          | Right tvars' <- mapM toTVar typs
          , all (`elem` tvars) tvars'
          -> pure (name, tvars')
      _ -> throwError $ unwords ["Bad data type definition: ", ppT typ]

typecheckInj :: Inj -> UIdent -> [TVar] -> Err (T.Inj' Type)
typecheckInj (Inj inj_name inj_typ) name tvars
    | not $ boundTVars tvars inj_typ
    = throwError ("Unbound type variables " ++ printTree name ++ " " ++ printTree tvars ++ " " ++ printTree inj_typ)
    | TData name' typs <- getDataId inj_typ
    , name'  == name
    , Right tvars' <- mapM toTVar typs
    , all (`elem` tvars) tvars'
    = pure $ T.Inj (coerce inj_name) (foldr TAll inj_typ tvars')
    | otherwise
    = throwError $ unwords
        ["Bad type constructor: ", show name
        , "\nExpected: ", ppT . TData name $ map TVar tvars
        , "\nActual: ", ppT $ getDataId inj_typ
        ]
  where
    boundTVars :: [TVar] -> Type -> Bool
    boundTVars tvars' = \case
        TAll tvar t  -> boundTVars (tvar:tvars') t
        TFun t1 t2   -> on (&&) (boundTVars tvars') t1 t2
        TVar tvar    -> elem tvar tvars'
        TData _ typs -> all (boundTVars tvars') typs
        TLit _       -> True
        TEVar _      -> error "TEVar in data type declaration"



---------------------------------------------------------------------------
-- * Typing rules
---------------------------------------------------------------------------

-- | Γ ⊢ e ↑ A ⊣ Δ
-- Under input context Γ, e checks against input type A, with output context ∆
check :: Exp -> Type -> Tc (T' T.Exp' Type)
check (ELit lit) t | t == typeof lit = apply (T.ELit lit, t)

--  Γ,α ⊢ e ↑ A ⊣ Δ,α,Θ
--  ------------------- ∀I
--  Γ ⊢ e ↑ ∀α.A ⊣ Δ
check e (TAll alpha a) = do
    let env_tvar = EnvTVar alpha
    insertEnv env_tvar
    e' <- check e a
    (env_l, _) <- gets (splitOn env_tvar . env)
    putEnv env_l
    apply e'

--  Γ,(x:A) ⊢ e ↑ B ⊢ Δ,(x:A),Θ
--  --------------------------- →I
--  Γ ⊢ λx.e ↑ A → B ⊣ Δ
check (EAbs x e) (TFun a b) = do
    let env_var = EnvVar x a
    insertEnv env_var
    e' <- check e b
    (env_l, _) <- gets (splitOn env_var . env)
    putEnv env_l
    apply (T.EAbs (coerce x) e', TFun a b)


--  Γ ⊢ e ↑ A ⊣ Θ  Θ ⊢ [Θ]A <: [Θ]C ⊣ Δ
--  ----------------------------------- CaseEmpty
--  Γ ⊢ case e of {} ↓ C ⊣ Δ

--                  Θ₁ ⊢ p₁⇒e₁ ↓ [Θ₁]C ⊣ Θ₂
--                        ...
--  Γ ⊢ e ↑ A ⊣ Θ₁  Θₙ ⊢ pₙ⇒eₙ ↓ [Θₙ]C ⊣ Δ
--  --------------------------------------- Case
--  Γ ⊢ case e of {p₁⇒e₁ ‥ pₙ⇒eₙ} ↓ C ⊣ Δ
check (ECase scrut branches) c = do
   (scrut', a) <- infer scrut
   case branches of
     [] -> do
         subtype a c
         apply (T.ECase (scrut', a) [], a)
     _  -> do
         branches' <- forM branches $ \(Branch p e) -> do
             p' <- checkPattern p =<< apply a
             e' <- check e c
             pure (T.Branch p' e')
         apply (T.ECase (scrut', a) branches', c)


--  Γ,α ⊢ e ↓ A ⊣ Θ   Θ ⊢ [Θ]A <: [Θ]B ⊣ Δ
--  -------------------------------------- Sub
--  Γ ⊢ e ↑ B ⊣ Δ
check e b = do
    (e', a) <- infer e
    b' <- apply b
    subtype a b'
    apply (e', b)


checkPattern :: Pattern -> Type -> Tc (T.Pattern' Type, Type)

--  ------------------- PVar
--  Γ ⊢ x ↑ A ⊣ Γ,(x:A)
checkPattern (PVar x) a = do
    insertEnv $ EnvVar x a
    apply (T.PVar (coerce x), a)

--  ------------- PCatch
--  Γ ⊢ _ ↑ A ⊣ Γ
checkPattern PCatch a = apply (T.PCatch, a)

--  A = typeof(lit)
--  ------------------------- PLit
--  Γ ⊢ lit ↑ A ⊣ Γ
checkPattern (PLit lit) a | a == typeof lit = apply (T.PLit lit, a)

--  Γ ∋ (K : T)  Γ ⊢ A <: B ⊣ Δ
--  ---------------------------
--  Γ ⊢ K ↑ T ⊣ Δ
checkPattern (PEnum k) b = do
    a <- maybeToRightM ("Unknown constructor " ++ show k) =<< lookupInj k
    subtype a b
    apply (T.PEnum (coerce k), a)





--
--  Γ ∋ (K : A)                                         Θ₂ ⊢ p₁ ↑ [Θ₁]A₁ ⊣ Θ₂
--  Γ ⊢ ∀ά₁‥άₘ A₁ → ‥ → Aₙ₊₁ = substituteAll(A) ⊣ Θ₁             ...
--  Θ₁ ⊢ Aₙ₊₁ <: B ⊣ Θ₂                               Θₙ₊₁ ⊢ pₙ ↑ [Θₙ₊₁]Aₙ ⊣ Δ
--  -----------------------------------------------------------------------
--  Γ ⊢ K p₁‥pₙ ↑ B ⊣ Δ
{- checkPattern (PInj k ps) b = do
    a <- maybeToRightM ("Unknown constructor " ++ show k) =<< lookupInj k
    a <- substituteAll a
    let as = getArgs a
    unless (length as == length ps) $ throwError "Wrong number of arguments!"
    ps' <- zipWithM (\p a -> checkPattern p =<< apply a) ps as
    apply (T.PInj (coerce k) ps', a)
  where
    substituteAll t = case t of
        TAll tvar t -> do
            tevar <- fresh
            substituteAll (substitute tvar tevar t)
        TFun t1 t2 -> onM TFun substituteAll t1 t2
        t -> pure t

    getArgs = \case
        TAll _ t -> getArgs t
        t        -> go [] t
      where
        go acc = \case
          TFun t1 t2 -> go (snoc t1 acc) t2
          _          -> acc -}







        --  Example
        --  Γ ∋ (K : A)  let A = ∀α. A₁ -> A₂ -> Tτs
        --  Γ ⊢ [ά/α]Tτs <: B     ⊣ Θ₁
        --  Θ  ⊢ p₁ ↑ [Θ][ά/α]A₁  ⊣ Θ₂
        --  Θ₂ ⊢ p₂ ↑ [Θ₂][ά/α]A₂ ⊣ Δ
        --  ---------------------------
        --  Γ ⊢ K p₁ p₂ ↑ B ⊣ Δ
checkPattern (PInj name ps) t_patt = do
            t_inj <- maybeToRightM "unknown constructor" =<< lookupInj name
            let ts = getArgs t_inj
            unless (length ts == length ps)
                $ throwError "Wrong number of arguments!"

            -- [ά/α]
            sub <- substituteTVarsOf t_inj
            subtype (sub $ getDataId t_inj) t_patt
            let check p t = checkPattern p =<< apply (sub t)
            ps' <- zipWithM check ps ts
            apply (T.PInj (coerce name) ps', t_patt)
          where
            substituteTVarsOf = \case
                TAll tvar t -> do
                    tevar <- fresh
                    (substitute tvar tevar .) <$> substituteTVarsOf t
                _ -> pure id

            getArgs = \case
                TAll _ t -> getArgs t
                t        -> go [] t
              where
                go acc = \case
                  TFun t1 t2 -> go (snoc t1 acc) t2
                  _          -> acc

-- | Γ ⊢ e ↓ A ⊣ Δ
-- Under input context Γ, e infers output type A, with output context ∆
infer :: Exp -> Tc (T' T.Exp' Type)
infer (ELit lit) = apply (T.ELit lit, typeof lit)

--  Γ ∋ (x : A)         Γ ⊢ rec(x)
--  ------------- Var   --------------------- VarRec
--  Γ ⊢ x ↓ A ⊣ Γ       Γ ⊢ x ↓ ά ⊣ Γ,(x : ά)
infer (EVar x) = do
    a <- ifM (gets $ (x==) . currentBind) varRec var
    apply (T.EVar (coerce x), a)
  where
    var = maybeToRightM "Can't infer" =<<
          liftA2 (<|>) (lookupEnv x) (lookupSig x)
    varRec = do
        alpha <- TEVar <$> fresh
        insertEnv (EnvVar x alpha)
        pure alpha

--  Γ ∋ (k : A)
--  ------------- Inj
--  Γ ⊢ k ↓ A ⊣ Γ
infer (EInj k) = do
    t <- maybeToRightM ("Unknown constructor: " ++ show k)
         =<< lookupInj k
    apply (T.EInj $ coerce k, t)

--  Γ ⊢ A   Γ ⊢ e ↑ A ⊣ Δ
--  --------------------- Anno
--  Γ ⊢ (e : A) ↓ A ⊣ Δ
infer (EAnn e a) = do
    _ <- gets $ (`wellFormed` a) . env
    (e', _) <- check e a
    apply (e', a)

--  Γ ⊢ e₁ ↓ A ⊣ Θ   Γ ⊢ [Θ]A • ⇓ C ⊣ Δ
--  ----------------------------------- →E
--  Γ ⊢ e₁ e₂ ↓ C ⊣ Δ
infer (EApp e1 e2) = do
    e1'@(_, a) <- infer e1
    (e2', c) <- applyInfer a e2
    apply (T.EApp e1' e2', c)

--  Γ,ά,έ,(x:ά) ⊢ e ↑ έ ⊣ Δ,(x:ά),Θ
--  ------------------------------- →I
--  Γ ⊢ λx.e ↓ ά → έ ⊣ Δ
infer (EAbs name e) = do
    alpha <- fresh
    epsilon <- fresh
    insertEnv $ EnvTEVar alpha
    insertEnv $ EnvTEVar epsilon
    let env_var = EnvVar name (TEVar alpha)
    insertEnv env_var
    e' <- check e $ TEVar epsilon
    dropTrailing env_var
    apply (T.EAbs (coerce name) e', on TFun TEVar alpha epsilon)

--  Γ ⊢ rhs ↓ A ⊣ Θ   Θ,(x:A) ⊢ e ↑ C ⊣ Δ,(x:A),Θ
--  -------------------------------------------- LetI
--  Γ ⊢ let x = rhs in e ↑ C ⊣ Δ
infer (ELet (Bind x vars rhs) e) = do
    (rhs', a) <- infer $ foldr EAbs rhs vars
    let env_var = EnvVar x a
    insertEnv env_var
    e'@(_, c) <- infer e
    (env_l, _) <- gets (splitOn env_var . env)
    putEnv env_l
    apply (T.ELet (T.Bind (coerce x, a) [] (rhs', a)) e', c)

--  Γ ⊢ e₁ ↑ Int ⊣ Θ  Θ ⊢ e₂ ↑ Int
--  --------------------------- +I
--  Γ ⊢ e₁ + e₂ ↓ Int ⊣ Δ
infer (EAdd e1 e2) = do
    e1' <- check e1 int
    e2' <- check e2 int
    apply (T.EAdd e1' e2', int)


--  Γ ⊢ e ↑ A ⊣ Δ
--  ------------------------ CaseEmpty↓
--  Γ ⊢ case e of {} ↑ A ⊣ Δ

--                  Θ₁ ⊢ p₁⇒e₁ ↓ [Θ₁]C ⊣ Θ₂
--                        ...
--  Γ ⊢ e ↑ A ⊣ Θ₁  Θₙ ⊢ pₙ⇒eₙ ↓ [Θₙ]C ⊣ Δ
--  --------------------------------------- Case↓
--  Γ ⊢ case e of {p₁⇒e₁ ‥ pₙ⇒eₙ} ↓ C ⊣ Δ
infer (ECase scrut branches) = do
   (scrut', a) <- infer scrut
   case branches of
     [] -> apply (T.ECase (scrut', a) [], a)
     (Branch _ e):_  -> do
         (_, b)<- infer e
         (branches', b') <- foldlM go ([], b) branches
         apply (T.ECase (scrut', a) branches', b')
       where
         go (pi, b) (Branch p e) = do
             p' <- checkPattern p =<< apply a
             e'@(_, b') <- infer e
             subtype b' b
             apply (T.Branch p' e' : pi, b')

-- | Γ ⊢ A • e ⇓ C ⊣ Δ
-- Under input context Γ , applying a function of type A to e infers type C, with output context ∆
-- Instantiate existential type variables until there is an arrow type.
applyInfer :: Type -> Exp -> Tc (T' T.Exp' Type, Type)

--  Γ,ά ⊢ [ά/α]A • e ⇓ C ⊣ Δ
--  ------------------------ ∀App
--  Γ ⊢ ∀α.A • e ⇓ C ⊣ Δ
applyInfer (TAll alpha a) e = do
    alpha' <- fresh
    insertEnv $ EnvTEVar alpha'
    applyInfer (substitute alpha alpha' a)  e

--  Γ[ά₂,ά₁,(ά=ά₁→ά₂)] ⊢ e ↑ ά₁ ⊣ Δ
--  ------------------------------- άApp
--  Γ[ά] ⊢ ά • e ⇓ ά₂ ⊣ Δ
applyInfer (TEVar alpha) e = do
    alpha1 <- fresh
    alpha2 <- fresh
    (env_l, env_r) <- gets (splitOn (EnvTEVar alpha) . env)
    putEnv $ (env_l
              :|> EnvTEVar alpha2
              :|> EnvTEVar alpha1
              :|> EnvTEVarSolved alpha (on TFun TEVar alpha1 alpha2)
             ) <> env_r
    e' <- check e $ TEVar alpha1
    apply (e', TEVar alpha2)

--  Γ ⊢ e ↑ A ⊣ Δ
--  --------------------- →App
--  Γ ⊢ A → C • e ⇓ C ⊣ Δ
applyInfer (TFun a c) e = do
    exp' <- check e a
    apply (exp', c)

applyInfer a e = throwError ("Cannot apply type " ++ show a ++ " with expression " ++ show e)

---------------------------------------------------------------------------
-- * Subtyping rules
---------------------------------------------------------------------------

-- | Γ ⊢ A <: B ⊣ Δ
-- Under input context Γ, type A is a subtype of B, with output context ∆
subtype :: Type -> Type -> Tc ()
subtype (TLit lit1) (TLit lit2) | lit1 == lit2 = pure ()

--  -------------------- <:Var
--  Γ[α] ⊢ α <: α ⊣ Γ[α]
subtype (TVar alpha) (TVar alpha') | alpha == alpha' = pure ()

--  -------------------- <:Exvar
--  Γ[ά] ⊢ ά <: ά ⊣ Γ[ά]
subtype (TEVar alpha) (TEVar alpha') | alpha == alpha' = pure ()

--  Γ ⊢ B₁ <: A₁ ⊣ Θ   Θ ⊢ [Θ]A₂ <: [Θ]B₂ ⊣ Δ
--  ----------------------------------------- <:→
--  Γ ⊢ A₁ → A₂ <: B₁ → B₂ ⊣ Δ
subtype (TFun a1 a2) (TFun b1 b2) = do
    subtype b1 a1
    a2' <- apply a2
    b2' <- apply b2
    subtype a2' b2'

--  Γ, α ⊢ A <: B ⊣ Δ,α,Θ
--  --------------------- <:∀R
--  Γ ⊢ A <: ∀α. B ⊣ Δ
subtype a (TAll alpha b) = do
    let env_tvar = EnvTVar alpha
    insertEnv env_tvar
    subtype a b
    dropTrailing env_tvar

--  Γ,▶ ά,ά ⊢ [ά/α]A <: B ⊣ Δ,▶ ά,Θ
--  ------------------------------- <:∀L
--  Γ ⊢ ∀α.A <: B ⊣ Δ
subtype (TAll alpha a) b = do
    alpha' <- fresh
    let env_marker = EnvMark alpha'
    insertEnv env_marker
    insertEnv $ EnvTEVar alpha'
    let a' = substitute alpha alpha' a
    subtype a' b
    dropTrailing env_marker

--  ά ∉ FV(A)   Γ[ά] ⊢ ά :=< A ⊣ Δ
--  ------------------------------ <:instantiateL
--  Γ[ά] ⊢ ά <: A ⊣ Δ
subtype (TEVar alpha) a | notElem alpha $ frees a = instantiateL alpha a

--  ά ∉ FV(A)   Γ[ά] ⊢ A =:< ά ⊣ Δ
--  ------------------------------ <:instantiateR
--  Γ[ά] ⊢ A <: ά ⊣ Δ
subtype a  (TEVar alpha) | notElem alpha $ frees a = instantiateR a alpha


subtype (TData t1 as) (TData t2 bs)

    --  -------------- <:TypeConEmpty
    --  Γ ⊢ T <: T ⊣ Γ
    | t1 == t2
    , [] <- as
    , [] <- bs
    = pure ()

    --    Γ ⊢ A₁ <: B₁ ⊣ Θ₁
    --           ...
    -- Θₙ₋₁ ⊢ [Θₙ₋₁]Aₙ <: [Θₙ₋₁]Bₙ ⊣ Δ
    -- -------------------------------- <:TypeCon
    -- Γ ⊢ T A₁ ‥ Aₙ <: T B₁ ‥ Bₙ ⊣ Δ
    | t1 == t2
    , a:as <- as
    , b:bs <- bs
    = do
    subtype a b
    zipWithM_ go as bs
  where
    go a b = do
        a' <- apply a
        b' <- apply b
        subtype a' b'

subtype (TIdent t1) (TIdent t2) | t1 == t2 = pure ()

subtype t1 t2 = throwError $ unwords ["Types", show t1, "and", show t2, "doesn't match!"]

---------------------------------------------------------------------------
-- * Instantiation rules
---------------------------------------------------------------------------

-- | Γ ⊢ ά :=< A ⊣ Δ
-- Under input context Γ, instantiate ά such that ά <: A, with output context ∆
instantiateL :: TEVar -> Type -> Tc ()
instantiateL alpha a = gets env >>= \env -> go env alpha a
  where
    go env alpha tau
       | isMono tau
       , (env_l, env_r) <- splitOn (EnvTEVar alpha) env
       , Right _ <-  wellFormed env_l tau
       = putEnv $ (env_l :|> EnvTEVarSolved alpha tau) <> env_r

    --  Γ ⊢ τ
    --  ----------------------------- InstLSolve
    --  Γ,ά,Γ' ⊢ ά :=< τ ⊣ Γ,(ά=τ),Γ'
    go env alpha tau
        | isMono tau
        , (env_l, env_r) <- splitOn (EnvTEVar alpha) env
        , Right _ <-  wellFormed env_l tau
        = putEnv $ (env_l :|> EnvTEVarSolved alpha tau) <> env_r

    --  ----------------------------- InstLReach
    --  Γ[ά][έ] ⊢ ά :=< έ ⊣ Γ[ά][έ=ά]
    go env alpha (TEVar epsilon) = do
        let (env_l, env_r) = splitOn (EnvTEVar epsilon) env
        putEnv $ (env_l :|> EnvTEVarSolved epsilon (TEVar alpha)) <> env_r

    --  Γ[ά₂ά₁,(ά=ά₁→ά₂)] ⊢ A₁ =:< ά₁ ⊣ Θ  Θ ⊢ ά₂ :=< [Θ]A₂ ⊣ Δ
    --  ------------------------------------------------------- InstLArr
    --  Γ[ά] ⊢ ά :=< A₁ → A₂ ⊣ Δ
    go _ alpha (TFun a1 a2) = do
        alpha1 <- fresh
        alpha2 <- fresh
        insertEnv $ EnvTEVar alpha2
        insertEnv $ EnvTEVar alpha1
        insertEnv $ EnvTEVarSolved alpha (on TFun TEVar alpha1 alpha2)
        instantiateR a1 alpha1
        instantiateL alpha2 =<< apply a2

    --  Γ[ά],ε ⊢ ά :=< E ⊣ Δ,ε,Δ'
    --  ------------------------- InstLAIIR
    --  Γ[ά] ⊢ ά :=< ∀ε.Ε ⊣ Δ
    go env tevar (TAll tvar t) = do
        instantiateL tevar t
        let (env_l, _) = splitOn (EnvTVar tvar) env
        putEnv env_l

    go _ alpha a = error $ "Trying to instantiateL: " ++ ppT (TEVar alpha)
                           ++ " <: " ++ ppT a

-- | Γ ⊢ A =:< ά ⊣ Δ
-- Under input context Γ, instantiate ά such that A <: ά, with output context ∆
instantiateR :: Type -> TEVar -> Tc ()
instantiateR a alpha = gets env >>= \env -> go env a alpha
  where
        --  Γ ⊢ τ
        --  ----------------------------- InstRSolve
        --  Γ,ά,Γ' ⊢ τ =:< ά ⊣ Γ,(ά=τ),Γ'
    go env tau alpha
        | isMono tau
        , (env_l, env_r) <- splitOn (EnvTEVar alpha) env
        , Right _ <- wellFormed env_l tau
        = putEnv $ (env_l :|> EnvTEVarSolved alpha tau) <> env_r

    --
    --  ----------------------------- InstRReach
    --  Γ[ά][έ] ⊢ έ =:< ά ⊣ Γ[ά][έ=ά]
    go env (TEVar epsilon) alpha = do
        let (env_l, env_r) = splitOn (EnvTEVar epsilon) env
        putEnv $ (env_l :|> EnvTEVarSolved epsilon (TEVar alpha)) <> env_r

    --  Γ[ά₂ά₁,(ά=ά₁→ά₂)] ⊢ A₁ :=< ά₁ ⊣ Θ  Θ ⊢ ά₂ =:< [Θ]A₂ ⊣ Δ
    --  ------------------------------------------------------- InstRArr
    --  Γ[ά] ⊢ A₁ → A₂ =:< ά ⊣ Δ
    go _ (TFun a1 a2) alpha = do
        alpha1 <- fresh
        alpha2 <- fresh
        insertEnv $ EnvTEVar alpha2
        insertEnv $ EnvTEVar alpha1
        insertEnv $ EnvTEVarSolved alpha (on TFun TEVar alpha1 alpha2)
        instantiateL alpha1 a1
        a2' <- apply a2
        instantiateR a2' alpha2

    --  Γ[ά],▶έ,ε ⊢ [έ/ε]E =:< ά ⊣ Δ,▶έ,Δ'
    --  ---------------------------------- InstRAIIL
    --  Γ[ά] ⊢ ∀ε.Ε =:< ά ⊣ Δ
    go env (TAll epsilon e) alpha = do
        epsilon' <- fresh
        insertEnv $ EnvMark epsilon'
        insertEnv $ EnvTVar epsilon
        instantiateR (substitute epsilon epsilon' e) alpha
        let (env_l, _) = splitOn (EnvMark epsilon') env
        putEnv env_l

    go _ a alpha = throwError $ "Trying to instantiateR: " ++ ppT a ++ " <: "
                           ++ ppT (TEVar alpha)

---------------------------------------------------------------------------
-- * Auxiliary
---------------------------------------------------------------------------

frees :: Type -> [TEVar]
frees = \case
  TLit _       -> []
  TVar _       -> []
  TEVar tevar  -> [tevar]
  TFun t1 t2   -> on (++) frees t1 t2
  TAll _ t     -> frees t
  TData _ typs -> concatMap frees typs

-- | [ά/α]A
substitute :: TVar  -- α
           -> TEVar -- ά
           -> Type  -- A
           -> Type  -- [ά/α]A
substitute tvar tevar typ = case typ of
    TLit _                     -> typ
    TVar tvar' | tvar' == tvar -> TEVar tevar
               | otherwise     -> typ
    TEVar _                    -> typ
    TFun t1 t2                 -> on TFun substitute' t1 t2
    TAll tvar' t               -> TAll tvar' (substitute' t)
    TData name typs            -> TData name $ map substitute' typs
  where
    substitute' = substitute tvar tevar

-- | Γ,x,Γ' → (Γ, Γ')
splitOn :: EnvElem -> Env -> (Env, Env)
splitOn x env = second (S.drop 1) $ S.breakl (==x) env

-- | Drop frontmost elements until and including element @x@.
dropTrailing :: EnvElem -> Tc ()
dropTrailing x = modifyEnv $ S.takeWhileL (/= x)


findSolved :: TEVar -> Env -> Maybe Type
findSolved _     Empty      = Nothing
findSolved tevar (xs :|> x) = case x of
    EnvTEVarSolved tevar' t | tevar == tevar' -> Just t
    _                                         -> findSolved tevar xs

-- | Γ ⊢ A
--   Under context Γ, type A is well-formed
wellFormed :: Env -> Type -> Err ()
wellFormed env = \case
    TLit _ -> pure ()

    --  -------- UvarWF
    --  Γ[α] ⊢ α
    TVar tvar -> unless (EnvTVar tvar `elem` env) $
                     throwError ("Unbound type variable: " ++ show tvar)
    --  Γ ⊢ A   Γ ⊢ B
    --  ------------- ArrowWF
    --  Γ ⊢ A → B
    TFun t1 t2 -> do { wellFormed env t1; wellFormed env t2 }

    --  Γ,α ⊢ A
    --  -------- ForallWF
    --  Γ ⊢ ∀α.A
    TAll tvar t -> wellFormed (env :|> EnvTVar tvar) t

    TEVar tevar
        --  ---------- EvarWF
        --  Γ[ά] ⊢ ά
        | EnvTEVar tevar `elem` env -> pure ()

        --  ---------- SolvedEvarWF
        --  Γ[ά=τ] ⊢ ά
        | Just _ <- findSolved tevar env -> pure ()
        | otherwise -> throwError ("Can't find type: " ++ show tevar)

    TData _ typs -> mapM_ (wellFormed env) typs

isMono :: Type -> Bool
isMono = \case
  TAll{}       -> False
  TFun t1 t2   -> on (&&) isMono t1 t2
  TData _ typs -> all isMono typs
  TVar _       -> True
  TEVar _      -> True
  TLit _       -> True

fresh :: Tc TEVar
fresh = do
   tevar <- gets (MkTEVar . LIdent . show . next_tevar)
   modify $ \cxt -> cxt { next_tevar = succ cxt.next_tevar }
   pure tevar


isComplete :: Env -> Bool
isComplete = isNothing . S.findIndexL unSolvedTEVar
  where
    unSolvedTEVar = \case
        EnvTEVar _ -> True
        _          -> False

getDataId :: Type -> Type
getDataId typ = case typ of
  TAll _ t -> getDataId t
  TFun _ t -> getDataId t
  TData {} -> typ

toTVar :: Type -> Err TVar
toTVar = \case
    TVar tvar -> pure tvar
    _         -> throwError "Not a type variable"

insertEnv :: EnvElem -> Tc ()
insertEnv x = modifyEnv (:|> x)

lookupSig :: LIdent -> Tc (Maybe Type)
lookupSig x = gets (Map.lookup x . sig)

insertSig :: LIdent -> Type -> Tc ()
insertSig name t = modify $ \cxt -> cxt { sig = Map.insert name t cxt.sig }

lookupEnv :: LIdent -> Tc (Maybe Type)
lookupEnv x = gets (findId . env)
  where
    findId Empty      = Nothing
    findId (ys :|> y) = case y of
        EnvVar x' t | x==x' -> Just t
        _                   -> findId ys

lookupInj :: UIdent -> Tc (Maybe Type)
lookupInj x = gets (Map.lookup x . data_injs)

putEnv :: Env -> Tc ()
putEnv = modifyEnv . const

modifyEnv :: (Env -> Env) -> Tc ()
modifyEnv f =
  modify $ \cxt -> {- trace (ppEnv (f cxt.env)) -} cxt { env = f cxt.env }

pattern DBind' name vars exp = DBind (Bind name vars exp)
pattern DSig' name typ = DSig (Sig name typ)

---------------------------------------------------------------------------
-- * Apply
---------------------------------------------------------------------------

class Apply a where
    apply :: a -> Tc a

instance Apply Type                         where apply = applyType
instance Apply (T.Exp' Type)                where apply = applyExp
instance Apply (T.Branch' Type)             where apply = applyBranch
instance Apply (T.Pattern' Type)            where apply = applyPattern
instance Apply a => Apply [a]               where apply = mapM apply
instance (Apply a, Apply b) => Apply (a, b) where apply = applyPair
instance Apply T.Ident                      where apply = pure

applyType :: Type -> Tc Type
applyType t = gets $ (`applyType'` t) . env

-- | [Γ]A. Applies context to type until fully applied.
applyType' :: Env -> Type -> Type
applyType' cxt typ | typ == typ' = typ'
                   | otherwise   = applyType' cxt typ'
  where
    typ' = case typ of
        TLit _          -> typ
        TData name typs -> TData name $ map (applyType' cxt) typs
        -- [Γ]α = α
        TVar _          -> typ
        -- [Γ[ά=τ]]ά = [Γ[ά=τ]]τ
        -- [Γ[ά]]ά = [Γ[ά]]ά
        TEVar tevar     -> fromMaybe typ $ findSolved tevar cxt
        -- [Γ](A → B) = [Γ]A → [Γ]B
        TFun t1 t2      -> on TFun (applyType' cxt) t1 t2
        -- [Γ](∀α. A) = (∀α. [Γ]A)
        TAll tvar t     -> TAll tvar $ applyType' cxt t
        TIdent t        -> typ

applyExp :: T.Exp' Type -> Tc (T.Exp' Type)
applyExp exp = case exp of
    T.ELet (T.Bind id vars rhs) exp -> do
        id <- apply id
        vars' <- mapM apply vars
        rhs' <- apply rhs
        exp' <- apply exp
        pure $ T.ELet (T.Bind id vars' rhs') exp'
    T.EApp e1 e2 -> liftA2 T.EApp (apply e1) (apply e2)
    T.EAdd e1 e2 -> liftA2 T.EAdd (apply e1) (apply e2)
    T.EAbs name e  -> T.EAbs name <$> apply e
    T.ECase e branches -> liftA2 T.ECase (apply e)
                                         (mapM apply branches)
    _ -> pure exp

applyBranch :: T.Branch' Type -> Tc (T.Branch' Type)
applyBranch (T.Branch (p, t) e) = do
    pt <- liftA2 (,) (apply p) (apply t)
    e' <- apply e
    pure $ T.Branch pt e'

applyPattern :: T.Pattern' Type -> Tc (T.Pattern' Type)
applyPattern = \case
    T.PVar id      -> T.PVar <$> apply id
    T.PInj name ps -> T.PInj name <$> apply ps
    p              -> pure p

applyPair :: (Apply a, Apply b) => (a, b) -> Tc (a, b)
applyPair (x, y) = liftA2 (,) (apply x) (apply y)

---------------------------------------------------------------------------
-- * Debug
---------------------------------------------------------------------------

traceEnv s = do
  env <- gets env
  trace (s ++ " " ++ ppEnv env) pure ()

traceD s x = trace (s ++ " " ++ show x) pure ()

traceT s x = trace (s ++ " : " ++ ppT x) pure ()

traceTs s xs = trace (s ++ " [ " ++ intercalate ", " (map ppT xs) ++ " ]") pure ()

ppT = \case
    TLit (UIdent s)            -> s
    TVar (MkTVar (LIdent s))   -> "tvar_" ++ s
    TFun t1 t2                 -> ppT t1 ++ "->" ++ ppT t2
    TAll (MkTVar (LIdent s)) t -> "forall " ++ s ++ ". " ++ ppT t
    TEVar (MkTEVar (LIdent s)) -> "tevar_" ++ s
    TData (UIdent name) typs   -> name ++ " (" ++ unwords (map ppT typs)
                                       ++ " )"
    TIdent (UIdent name)       -> name

ppEnvElem = \case
   EnvVar         (LIdent s) t           -> s ++ ":" ++ ppT t
   EnvTVar        (MkTVar  (LIdent s))   -> "tvar_" ++ s
   EnvTEVar       (MkTEVar (LIdent s))   -> "tevar_" ++ s
   EnvTEVarSolved (MkTEVar (LIdent s)) t -> "tevar_" ++ s ++ "=" ++ ppT t
   EnvMark        (MkTEVar (LIdent s))   -> "▶" ++ "tevar_" ++ s

ppEnv = \case
    Empty      -> "·"
    (xs :|> x) -> ppEnv xs ++ " (" ++ ppEnvElem x ++ ")"
