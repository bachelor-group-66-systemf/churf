{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeChecker.TypeCheckerBidir (typecheck, getVars) where

import           Auxiliary                 (maybeToRightM, snoc)
import           Control.Applicative       (Alternative, Applicative (liftA2),
                                            (<|>))
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            liftEither, runExceptT, unless,
                                            zipWithM, zipWithM_)
import           Control.Monad.State       (MonadState (get, put), State,
                                            evalState, gets, modify)
import           Data.Coerce               (coerce)
import           Data.Either.Combinators   (maybeToRight)
import           Data.Function             (on)
import           Data.List                 (intercalate)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isNothing)
import           Data.Sequence             (Seq (..))
import qualified Data.Sequence             as S
import           Data.Tuple.Extra          (second, secondM)
import           Debug.Trace               (trace)
import           Grammar.Abs
import           Grammar.ErrM
import           Grammar.Print             (printTree)
import           Prelude                   hiding (exp, id)
import qualified TypeChecker.TypeCheckerIr as T

-- Implementation is derived from the paper (Dunfield and Krishnaswami 2013)
-- https://doi.org/10.1145/2500365.2500582

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
    { env        :: Env                                  -- ^ Local scope context  Γ
    , sig        :: Map LIdent Type                      -- ^ Top-level signatures x : A
    , binds      :: Map LIdent Exp                       -- ^ Top-level binds x : e
    , next_tevar :: Int                                  -- ^ Counter to distinguish ά
    , data_injs  :: Map UIdent Type                      -- ^ Data injections (constructors) K/inj : A
    , data_types :: Map UIdent (Type, [(UIdent, Type)])  -- ^ Data types (∀α. D (α), K₁:A₁ + ‥ + Kₙ:Aₙ)
    } deriving (Show, Eq)

newtype Tc a = Tc { runTc :: ExceptT String (State Cxt) a }
    deriving (Functor, Applicative, Monad, Alternative, MonadState Cxt, MonadError String)

typecheck :: Program -> Err (T.Program' Type)
typecheck (Program defs) = do
    datatypes <- mapM typecheckDataType [ d | DData d <- defs ]


    let initCxt = Cxt
            { env          = mempty
            , sig          = Map.fromList [ (name, t)
                                          | DSig' name t <- defs
                                          ]
            , binds        = Map.fromList [ (name, foldr EAbs rhs vars)
                                          | DBind' name vars rhs <- defs
                                          ]
            , next_tevar   = 0
            , data_injs = Map.fromList [ (name, foldr ($) typ $ getForallsData typ)
                                       | Data _ injs <- datatypes
                                       , Inj name typ <- injs
                                       ]
            , data_types = Map.fromList [ let
                                            TData name _ = getTData typ
                                            kts = [(k,t) | Inj k t <- injs ]
                                          in
                                            (name, (typ, kts))
                                        | Data typ injs <- datatypes
                                        ]
            }

    binds' <- evalState (runExceptT (runTc $ mapM typecheckBind binds)) initCxt;
    pure . T.Program $ map T.DData (coerceData datatypes) ++ map T.DBind binds'
  where
    binds = [ b | DBind b <- defs ]
    -- TODO this should happen in typecheckDataType
    coerceData = map (\(Data t injs) -> T.Data t $ map
                     (\(Inj name typ) -> T.Inj (coerce name) typ) injs)


typecheckBind :: Bind -> Tc (T.Bind' Type)
typecheckBind (Bind name vars rhs) = do
    bind' <- lookupSig name >>= \case
        -- TODO These Judgment aren't accurate
        -- (f:A → B) ∈ Γ
        -- Γ,(xs:A) ⊢ e ↑ Β ⊣ Δ
        ---------------------------
        -- Γ ⊢ f xs = e ↓ Α → B ⊣ Δ
        Just t  -> do
            (rhs', _) <- check (foldr EAbs rhs vars) t
            pure (T.Bind (coerce name, t) (coerce vars') (rhs', t))
          where
            vars' = zip vars $ getVars  t

        --  Γ ⊢ (λxs. e) ↓ A → B ⊣ Δ
        --  ------------------------------
        --  Γ ⊢ f xs = e ↓ [Γ]A → [Γ]B ⊣ Δ
        Nothing -> do
            (e, t) <- infer $ foldr EAbs rhs vars
            t'     <- applyEnv t
            e'     <- applyEnvExp e
            let rhs'  = skipLambdas (length vars) e'
                vars' = zip vars  $ getVars t'
            pure (T.Bind (coerce name, t') (coerce vars') (rhs', t'))
    env <- gets env
    unless (isComplete env) err
    putEnv Empty
    pure bind'
  where
    err = throwError $ unlines
        [ "Type inference failed: " ++ printTree (Bind name vars rhs)
        , "Did you forget to add type annotation to a polymorphic function?"
        ]

typecheckDataType :: Data -> Err Data
typecheckDataType (Data typ injs) = do
    (name, tvars) <- go [] typ
    injs' <- mapM (\i -> typecheckInj i name tvars) injs
    pure (Data typ injs')
  where
    go tvars = \case
      TAll tvar t -> go (tvar:tvars) t
      TData name typs
          | Right tvars' <- mapM toTVar typs
          , all (`elem` tvars) tvars'
          -> pure (name, tvars')
      _ -> throwError $ unwords ["Bad data type definition: ", ppT typ]

typecheckInj :: Inj -> UIdent -> [TVar] -> Err Inj
typecheckInj (Inj inj_name inj_typ) name tvars
    | not $ boundTVars tvars inj_typ
    = throwError "Unbound type variables"
    | TData name' typs <- getReturn inj_typ
    , name'  == name
    , Right tvars' <- mapM toTVar typs
    , all (`elem` tvars) tvars'
    = pure (Inj inj_name inj_typ)
    | otherwise
    = throwError $ unwords
        ["Bad type constructor: ", show name
        , "\nExpected: ", ppT . TData name $ map TVar tvars
        , "\nActual: ", ppT $ getReturn inj_typ
        ]
  where
    boundTVars :: [TVar] -> Type -> Bool
    boundTVars tvars' = \case
        TAll tvar t  -> boundTVars (tvar:tvars') t
        TFun t1 t2   -> on (&&) (boundTVars tvars') t1 t2
        TVar tvar    -> elem tvar tvars'
        TData _ typs -> all (boundTVars tvars) typs
        TLit _       -> True
        TEVar _      -> error "TEVar in data type declaration"

---------------------------------------------------------------------------
-- * Subtyping rules
---------------------------------------------------------------------------

-- | Γ ⊢ A <: B ⊣ Δ
-- Under input context Γ, type A is a subtype of B, with output context ∆
subtype :: Type -> Type -> Tc ()
subtype t1 t2 = case (t1, t2) of

    (TLit lit1, TLit lit2) | lit1 == lit2 -> pure ()

    --  -------------------- <:Var
    --  Γ[α] ⊢ α <: α ⊣ Γ[α]
    (TVar tvar1, TVar tvar2) | tvar1 == tvar2 -> pure ()

    --  -------------------- <:Exvar
    --  Γ[ά] ⊢ ά <: ά ⊣ Γ[ά]
    (TEVar tevar1, TEVar tevar2) | tevar1 == tevar2 -> pure ()

    --  Γ ⊢ B₁ <: A₁ ⊣ Θ   Θ ⊢ [Θ]A₂ <: [Θ]B₂ ⊣ Δ
    --  ----------------------------------------- <:→
    --  Γ ⊢ A₁ → A₂ <: B₁ → B₂ ⊣ Δ
    (TFun a1 a2, TFun b1 b2) -> do
        subtype b1 a1
        a2' <- applyEnv a2
        b2' <- applyEnv b2
        subtype a2' b2'

    --  Γ, α ⊢ A <: B ⊣ Δ,α,Θ
    --  --------------------- <:∀R
    --  Γ ⊢ A <: ∀α. B ⊣ Δ
    (a, TAll tvar b) -> do
        let env_tvar = EnvTVar tvar
        insertEnv env_tvar
        subtype a b
        dropTrailing env_tvar

    --  Γ,▶ ά,ά ⊢ [ά/α]A <: B ⊣ Δ,▶ ά,Θ
    --  ------------------------------- <:∀L
    --  Γ ⊢ ∀α.A <: B ⊣ Δ
    (TAll tvar a, b) -> do
        tevar <- fresh
        let env_marker = EnvMark tevar
            env_tevar  = EnvTEVar tevar
        insertEnv env_marker
        insertEnv env_tevar
        let a' = substitute tvar tevar a
        subtype a' b
        dropTrailing env_marker

    --  ά ∉ FV(A)   Γ[ά] ⊢ ά :=< A ⊣ Δ
    --  ------------------------------ <:instantiateL
    --  Γ[ά] ⊢ ά <: A ⊣ Δ
    (TEVar tevar, typ) | notElem tevar $ frees typ -> instantiateL tevar typ

    --  ά ∉ FV(A)   Γ[ά] ⊢ A =:< ά ⊣ Δ
    --  ------------------------------ <:instantiateR
    --  Γ[ά] ⊢ A <: ά ⊣ Δ
    (typ, TEVar tevar) | notElem tevar $ frees typ -> instantiateR typ tevar


    (TData name1 typs1, TData name2 typs2)

      --  D₁ = D₂
      --  ----------------
      --  Γ ⊢ D₁ () <: D₂ ()
      | name1 == name2
      , [] <- typs1
      , [] <- typs2
      -> pure ()

      --                    Γ ⊢ ά₁ <: έ₁ ⊣ Θ₁
      --                           ...
      -- D₁ = D₂   Θₙ₋₁ ⊢ [Θₙ₋₁]άₙ <: [Θₙ₋₁]έₙ ⊣ Δ
      -- -------------------------------------------
      -- Γ ⊢ D (ά₁ ‥ άₙ) <: D (έ₁ ‥ έₙ) ⊣ Δ
      | name1 == name2
      , t1:t1s <- typs1
      , t2:t2s <- typs2
      -> do
          subtype t1 t2
          zipWithM_ go t1s t2s
     where
       go t1' t2' = do
           t1'' <- applyEnv t1'
           t2'' <- applyEnv t2'
           subtype t1'' t2''

    _ -> throwError $ unwords ["Types", ppT t1, "and", ppT t2, "doesn't match!"]

---------------------------------------------------------------------------
-- * Instantiation rules
---------------------------------------------------------------------------

-- | Γ ⊢ ά :=< A ⊣ Δ
-- Under input context Γ, instantiate ά such that ά <: A, with output context ∆
instantiateL :: TEVar -> Type -> Tc ()
instantiateL tevar typ = gets env >>= go
  where
    go env

        --  Γ ⊢ τ
        --  ----------------------------- InstLSolve
        --  Γ,ά,Γ' ⊢ ά :=< τ ⊣ Γ,(ά=τ),Γ'
        | isMono typ
        , (env_l, env_r) <- splitOn (EnvTEVar tevar) env
        , Right _ <- wellFormed env_l typ
        = putEnv $ (env_l :|> EnvTEVarSolved tevar typ) <> env_r

        | TEVar tevar' <- typ = instReach tevar tevar'

        --  Γ[ά₂ά₁,(ά=ά₁→ά₂)] ⊢ A₁ =:< ά₁ ⊣ Θ  Θ ⊢ ά₂ :=< [Θ]A₂ ⊣ Δ
        --  ------------------------------------------------------- InstLArr
        --  Γ[ά] ⊢ ά :=< A₁ → A₂ ⊣ Δ
        | TFun t1 t2 <- typ = do
            tevar1 <- fresh
            tevar2 <- fresh
            insertEnv $ EnvTEVar tevar2
            insertEnv $ EnvTEVar tevar1
            insertEnv $ EnvTEVarSolved tevar (on TFun TEVar tevar1 tevar2)
            instantiateR t1 tevar1
            instantiateL tevar2 =<< applyEnv t2

        --  Γ[ά],ε ⊢ ά :=< E ⊣ Δ,ε,Δ'
        --  ------------------------- InstLAIIR
        --  Γ[ά] ⊢ ά :=< ∀ε.Ε ⊣ Δ
        | TAll tvar t <- typ = do
            instantiateL tevar t
            let (env_l, _) = splitOn (EnvTVar tvar) env
            putEnv env_l

        | otherwise = error $ "Trying to instantiateL: " ++ ppT (TEVar tevar)
                              ++ " <: " ++ ppT typ

-- | Γ ⊢ A =:< ά ⊣ Δ
-- Under input context Γ, instantiate ά such that A <: ά, with output context ∆
instantiateR :: Type -> TEVar  -> Tc ()
instantiateR typ tevar = gets env >>= go
  where
    go env

        --  Γ ⊢ τ
        --  ----------------------------- InstRSolve
        --  Γ,ά,Γ' ⊢ τ =:< ά ⊣ Γ,(ά=τ),Γ'
        | isMono typ
        , (env_l, env_r) <- splitOn (EnvTEVar tevar) env
        , Right _ <- wellFormed env_l typ
        = putEnv $ (env_l :|> EnvTEVarSolved tevar typ) <> env_r

        | TEVar tevar' <- typ = instReach tevar tevar'

        --  Γ[ά₂ά₁,(ά=ά₁→ά₂)] ⊢ A₁ :=< ά₁ ⊣ Θ  Θ ⊢ ά₂ =:< [Θ]A₂ ⊣ Δ
        --  ------------------------------------------------------- InstRArr
        --  Γ[ά] ⊢ ά =:< A₁ → A₂ ⊣ Δ
        | TFun t1 t2 <- typ = do
            tevar1 <- fresh
            tevar2 <- fresh
            insertEnv $ EnvTEVar tevar2
            insertEnv $ EnvTEVar tevar1
            insertEnv $ EnvTEVarSolved tevar (on TFun TEVar tevar1 tevar2)
            instantiateL tevar1 t1
            t2' <- applyEnv t2
            instantiateR t2' tevar2

        --  Γ[ά],▶έ,ε ⊢ [έ/ε]E =:< ά ⊣ Δ,▶έ,Δ'
        --  ---------------------------------- InstRAIIL
        --  Γ[ά] ⊢ ∀ε.Ε =:< ά ⊣ Δ
        | TAll tvar t <- typ = do
            tevar' <- fresh
            insertEnv $ EnvMark tevar'
            insertEnv $ EnvTVar tvar
            let t' = substitute tvar tevar' t
            instantiateR t' tevar
            let (env_l, _) = splitOn (EnvTVar tvar) env
            putEnv env_l


        | otherwise = error $ "Trying to instantiateR: " ++ ppT typ ++ " <: "
                              ++ ppT (TEVar tevar)


--  ----------------------------- InstLReach
--  Γ[ά][έ] ⊢ ά :=< έ ⊣ Γ[ά][έ=ά]
--
--  ----------------------------- InstRReach
--  Γ[ά][έ] ⊢ έ =:< ά ⊣ Γ[ά][έ=ά]
instReach :: TEVar -> TEVar -> Tc ()
instReach tevar tevar' = do
    (env_l, env_r) <- gets (splitOn (EnvTEVar tevar') . env)
    let env_solved =  EnvTEVarSolved tevar' $ TEVar tevar
    putEnv $ (env_l :|> env_solved) <> env_r

---------------------------------------------------------------------------
-- * Typing rules
---------------------------------------------------------------------------

-- | Γ ⊢ e ↑ A ⊣ Δ
-- Under input context Γ, e checks against input type A, with output context ∆
check :: Exp -> Type -> Tc (T.ExpT' Type)
check exp typ

    --  Γ,α ⊢ e ↑ A ⊣ Δ,α,Θ
    --  ------------------- ∀I
    --  Γ ⊢ e ↑ ∀α.A ⊣ Δ
    | TAll tvar t <- typ = do
        let env_tvar = EnvTVar tvar
        insertEnv env_tvar
        exp' <- check exp t
        (env_l, _) <- gets (splitOn env_tvar . env)
        putEnv env_l
        pure exp'

    --  Γ,(x:A) ⊢ e ↑ B ⊢ Δ,(x:A),Θ
    --  --------------------------- →I
    --  Γ ⊢ λx.e ↑ A → B ⊣ Δ
    | EAbs name e <- exp
    , TFun t1 t2  <- typ = do
        let env_id = EnvVar name t1
        insertEnv env_id
        e' <- check e t2
        (env_l, _) <- gets (splitOn env_id . env)
        putEnv env_l
        pure (T.EAbs (coerce name) e', typ)

    --                  Θ ⊢ Π ∷ [Θ]A ↑ [Θ]C ⊣ Δ
    --  Γ ⊢ e ↓ A ⊣ Θ   Δ ⊢ Π covers [Δ]A TODO
    --  ---------------------------------------
    --  Γ ⊢ case e of Π ↑ C ⊣ Δ
      | ECase scrut branches <- exp = do
          (scrut', t_scrut) <- infer scrut
          t_scrut'  <- applyEnv t_scrut
          typ'      <- applyEnv typ
          branches' <- mapM (\b -> checkBranch b t_scrut' typ') branches
          pure (T.ECase (scrut', t_scrut') branches', typ')

    | otherwise = subsumption
  where
    --  Γ,α ⊢ e ↓ A ⊣ Θ   Θ ⊢ [Θ]A <: [Θ]B ⊣ Δ
    --  -------------------------------------- Sub
    --  Γ ⊢ e ↑ B ⊣ Δ
    subsumption = do
      (exp', t) <- infer exp
      exp'' <- applyEnvExp exp'
      t'   <- applyEnv t
      typ' <- applyEnv typ
      subtype t' typ'
      pure (exp'', t')

-- | Γ ⊢ e ↓ A ⊣ Δ
-- Under input context Γ, e infers output type A, with output context ∆
infer :: Exp -> Tc (T.ExpT' Type)
infer = \case

    ELit lit -> pure (T.ELit lit, inferLit lit)

    --  (x : A) ∈ Γ
    --  ------------- Var
    --  Γ ⊢ x ↓ A ⊣ Γ
    EVar name -> do
        t <- liftA2 (<|>) (lookupEnv name) (lookupSig name) >>= \case
                 Just t  -> pure t
                 Nothing -> do
                     e <- maybeToRightM
                            ("Unbound variable " ++ show name)
                             =<< lookupBind name
                     snd <$> infer e
        pure (T.EVar (coerce name), t)

    EInj name -> do
        t <- maybeToRightM ("Unknown constructor: " ++ show name) =<< lookupInj name
        pure (T.EInj $ coerce name, t)

    --  Γ ⊢ A   Γ ⊢ e ↑ A ⊣ Δ
    --  --------------------- Anno
    --  Γ ⊢ (e : A) ↓ A ⊣ Δ
    EAnn e t -> do
        _ <- gets $ (`wellFormed` t) . env
        (e', _) <- check e t
        pure (e', t)

    --  Γ ⊢ e₁ ↓ A ⊣ Θ   Γ ⊢ [Θ]A • ⇓ C ⊣ Δ
    --  ----------------------------------- →E
    --  Γ ⊢ e₁ e₂ ↓ C ⊣ Δ
    EApp e1 e2 -> do
        (e1', t) <- infer e1
        t' <- applyEnv t
        e1'' <- applyEnvExp e1'
        (e2', t'') <- apply t' e2
        pure (T.EApp (e1'', t) e2', t'')

    --  Γ,ά,έ,(x:ά) ⊢ e ↑ έ ⊣ Δ,(x:ά),Θ
    --  ------------------------------- →I
    --  Γ ⊢ λx.e ↓ ά → έ ⊣ Δ
    EAbs name e -> do
      tevar1 <- fresh
      tevar2 <- fresh
      insertEnv $ EnvTEVar tevar1
      insertEnv $ EnvTEVar tevar2
      let env_id = EnvVar name (TEVar tevar1)
      insertEnv env_id
      e' <- check e $ TEVar tevar2
      dropTrailing env_id
      let t_exp = on TFun TEVar tevar1 tevar2
      pure (T.EAbs (coerce name) e', t_exp)


    --  Γ ⊢ e ↓ A ⊣ Θ   Θ,(x:A) ⊢ e' ↑ C ⊣ Δ,(x:A),Θ
    --  -------------------------------------------- LetI
    --  Γ ⊢ let x=e in e' ↑ C ⊣ Δ
    ELet (Bind name [] rhs) e -> do -- TODO vars
        (rhs', t_rhs) <- infer rhs
        let env_id = EnvVar name t_rhs
        insertEnv env_id
        (e', t) <- infer e
        (env_l, _) <- gets (splitOn env_id . env)
        putEnv env_l
        pure (T.ELet (T.Bind (coerce name, t_rhs) [] (rhs', t_rhs)) (e',t), t)

    --  Γ ⊢ e₁ ↑ Int   Γ ⊢ e₁ ↑ Int
    --  --------------------------- +I
    --  Γ ⊢ e₁ + e₂ ↓ Int ⊣ Δ
    EAdd e1 e2 -> do
        cxt <- get
        let t = TLit "Int"
        e1' <- check e1 t
        put cxt
        e2' <- check e2 t
        pure (T.EAdd e1' e2', t)

-- | Γ ⊢ A • e ⇓ C ⊣ Δ
-- Under input context Γ , applying a function of type A to e infers type C, with output context ∆
-- Instantiate existential type variables until there is an arrow type.
apply :: Type -> Exp -> Tc (T.ExpT' Type, Type)
apply typ exp = case typ of

    --  Γ,ά ⊢ [ά/α]A • e ⇓ C ⊣ Δ
    --  ------------------------ ∀App
    --  Γ ⊢ ∀α.A • e ⇓ C ⊣ Δ
    TAll tvar t -> do
        tevar <- fresh
        insertEnv $ EnvTEVar tevar
        let t' = substitute tvar tevar t
        apply t' exp

    --  Γ[ά₂,ά₁,(ά=ά₁→ά₂)] ⊢ e ↑ ά₁ ⊣ Δ
    --  ------------------------------- άApp
    --  Γ[ά] ⊢ ά • e ⇓ ά₂ ⊣ Δ
    TEVar tevar -> do
        tevar1 <- fresh
        tevar2 <- fresh
        let env_tevar1       = EnvTEVar tevar1
            env_tevar2       = EnvTEVar tevar2
            t_fun            = on TFun TEVar tevar1 tevar2
            env_tevar_solved = EnvTEVarSolved tevar t_fun
        (env_l, env_r) <- gets (splitOn (EnvTEVar tevar) . env)
        putEnv $
            (env_l :|> env_tevar2 :|> env_tevar1 :|> env_tevar_solved) <> env_r
        expT' <- check exp $ TEVar tevar1
        pure (expT', TEVar tevar2)

    --  Γ ⊢ e ↑ A ⊣ Δ
    --  --------------------- →App
    --  Γ ⊢ A → C • e ⇓ C ⊣ Δ
    TFun t1 t2 -> do
        expt' <- check exp t1
        pure (expt', t2)

    _ -> throwError ("Cannot apply type " ++ show typ ++ " with expression " ++ show exp)

---------------------------------------------------------------------------
-- * Pattern matching
---------------------------------------------------------------------------

-- | Γ ⊢ p ⇒ e ∷ A ↑ C
-- Under context Γ, check branch p ⇒ e of type A and bodies of type C
checkBranch :: Branch -> Type -> Type -> Tc (T.Branch' Type)
checkBranch (Branch patt exp) t_patt t_exp = do
    env_marker <- EnvMark <$> fresh
    insertEnv env_marker
    patt' <- checkPattern patt t_patt
    t_exp' <- applyEnv t_exp
    (exp, t_exp) <- check exp t_exp'
    (env_l, _) <- gets (splitOn env_marker . env)
    putEnv env_l
    pure (T.Branch patt' (exp, t_exp))

checkPattern :: Pattern -> Type -> Tc (T.Pattern' Type, Type)
checkPattern patt t_patt = (, t_patt) <$> case patt of
        PVar x -> do
            insertEnv $ EnvVar x t_patt
            pure $ T.PVar (coerce x, t_patt)
        PCatch -> pure T.PCatch
        PLit lit | inferLit lit == t_patt -> pure $ T.PLit (lit, t_patt)
                 | otherwise -> throwError "Literal in pattern have wrong type"

        PEnum name -> do
            t <- maybeToRightM ("Unknown constructor " ++ show name)
                     =<< lookupInj name
            subtype t t_patt
            pure $ T.PEnum (coerce name)



        --                                          Θ₁ ⊢ p₁ ↑ [Θ₁]B₁ ⊣ Θ₂
        --  Γ ⊢ (xₖ : B₁ → ‥ → Bₘ₊₁) ∈ Γ                     ...
        --  Γ ⊢ B₁ → ‥ → Bₘ₊₁ <: A₁ + ‥ + Aₙ ⊣ Θ₁   Θₘ ⊢ pₘ ↑ [Θₘ₋₁]Bₘ ⊣ Δ
        --  --------------------------------------------------------------
        --  Γ ⊢ injₖ xₖ. p₁ ‥ pₘ ↑ A₁ + ‥ + Aₙ ⊣ Δ
        PInj name ps -> undefined
            -- injs <- maybeToRightM err1 =<< lookupDataType name_d
            -- tinj <- liftEither . maybeToRight err2 $ lookup name injs
            -- trace (show $ length foralls) pure ()
            -- (tinj', tdata) <- substituteTVars foralls tinj tdata
            -- traceT "tinj'" tinj'
            -- traceT "tdata" tdata
            -- subtype (getTData $ getReturn tinj') tdata
            -- t_inj'' <- applyEnv tinj'
            -- tdata'  <- applyEnv tdata
            -- pure $ T.PInj (coerce name) []
          where
            substituteTVars fas t1 t2 = case fas of
                []      -> pure (t1, t2)
                fa:fas' -> do
                  (t1', t2') <- go fa (t1, t2)
                  substituteTVars fas' t1' t2'
              where
                go fa (t1, t2) = let TAll tvar _ = fa dummy in do
                  tevar <- fresh
                  insertEnv (EnvTEVar tevar)
                  traceT "tevar:" (TEVar tevar)
                  pure $ on (,) (substitute tvar tevar) t1 t2

            (foralls, tdata@(TData name_d _)) = partitionData t_patt
            err1 = unwords ["Unknown data type", show name_d]
            err2 = unwords ["No", show name, "constructor for data type", show name_d]


            -- §ps' <- zipWithM (\p t -> checkPattern p =<< applyEnv t) ps t_ps
            -- let ps'' = map fst ps' -- TODO
            -- pure $ T.PInj (coerce name) []


    -- TAll tvar t -> do
    --     tevar <- fresh
    --     let -- env_marker = EnvMark tevar
    --         env_tevar  = EnvTEVar tevar
    --     -- insertEnv env_marker
    --     insertEnv env_tevar
    --     let a' = substitute tvar tevar a
    --     subtype a' b
    --     -- dropTrailing env_marker

    -- TData name_d typs -> do
    --
    --     subtype t_k typ
    --     undefined
    --   where

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

applyEnvExp :: T.Exp' Type -> Tc (T.Exp' Type)
applyEnvExp exp = case exp of
    T.ELet (T.Bind id vars rhs) exp -> do
        id <- applyEnvId id
        vars' <- mapM applyEnvId vars
        rhs' <- applyEnvExpT rhs
        exp' <- applyEnvExpT exp
        pure $ T.ELet (T.Bind id vars' rhs') exp'
    T.EApp e1 e2 -> liftA2 T.EApp (applyEnvExpT e1) (applyEnvExpT e2)
    T.EAdd e1 e2 -> liftA2 T.EAdd (applyEnvExpT e1) (applyEnvExpT e2)
    T.EAbs name e  -> T.EAbs name <$> applyEnvExpT e
    T.ECase e branches -> liftA2 T.ECase (applyEnvExpT e)
                                         (mapM applyEnvBranch branches)
    _ -> pure exp
  where
    applyEnvExpT (e, t) = liftA2 (,) (applyEnvExp e) (applyEnv t)
    applyEnvId = secondM applyEnv
    applyEnvBranch (T.Branch (p, t) e) = do
        pt <- liftA2 (,) (applyEnvPattern p) (applyEnv t)
        e' <- applyEnvExpT e
        pure $ T.Branch pt e'
    applyEnvPattern = \case
        T.PVar id       -> T.PVar <$> applyEnvId id
        T.PLit (lit, t) -> T.PLit . (lit, ) <$> applyEnv t
        T.PInj name ps  -> T.PInj name <$> mapM applyEnvPattern ps
        p               -> pure p

applyEnv :: Type -> Tc Type
applyEnv t = gets $ (`applyEnv'` t) . env

-- | [Γ]A. Applies context to type until fully applied.
applyEnv' :: Env -> Type -> Type
applyEnv' cxt typ | typ == typ' = typ'
                  | otherwise   = applyEnv' cxt typ'
  where
    typ' = case typ of
        TLit _          -> typ
        TData name typs -> TData name $ map (applyEnv' cxt) typs
        -- [Γ]α = α
        TVar _          -> typ
        -- [Γ[ά=τ]]ά = [Γ[ά=τ]]τ
        -- [Γ[ά]]ά = [Γ[ά]]ά
        TEVar tevar     -> fromMaybe typ $ findSolved tevar cxt
        -- [Γ](A → B) = [Γ]A → [Γ]B
        TFun t1 t2      -> on TFun (applyEnv' cxt) t1 t2
        -- [Γ](∀α. A) = (∀α. [Γ]A)
        TAll tvar t     -> TAll tvar $ applyEnv' cxt t

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

inferLit :: Lit -> Type
inferLit = \case
    LInt  _ -> TLit "Int"
    LChar _ -> TLit "Char"

fresh :: Tc TEVar
fresh = do
   tevar <- gets (MkTEVar . LIdent . ("a#" ++) . show . next_tevar)
   modify $ \cxt -> cxt { next_tevar = succ cxt.next_tevar }
   pure tevar

getVars :: Type -> [Type]
getVars = fst . partitionType

getReturn :: Type -> Type
getReturn = snd . partitionType

-- | Partion type into variable types and return type.
--
--  ∀a.∀b. a → (∀c. c → c) → b
--  ([a, ∀c. c → c], b)
--
--  Unsure if foralls should be added to the return type or not.
--  FIXME
partitionType :: Type -> ([Type], Type)
partitionType = go [] . skipForalls'
  where

    go acc t = case t of
        TFun t1 t2 -> go (snoc t1 acc) t2
        _          -> (acc, t)

skipForalls' :: Type -> Type
skipForalls' = snd . skipForalls

skipForalls :: Type -> ([Type -> Type], Type)
skipForalls = go []
  where
    go acc typ = case typ of
        TAll tvar t -> go (snoc (TAll tvar) acc) t
        _           -> (acc, typ)


getForallsData :: Type -> [Type -> Type]
getForallsData = fst . partitionData

getTData :: Type -> Type
getTData = snd . partitionData

partitionData :: Type -> ([Type -> Type], Type)
partitionData = go . ([],)
  where
    go (acc, typ) = case typ of
        TAll tvar t -> go (snoc (TAll tvar) acc, t)
        TData {}    -> (acc, typ)
        _           -> error "Bad data type"


partitionTypeWithForall :: Type -> ([Type], Type)
partitionTypeWithForall typ = (t_vars', t_return')
  where
    t_vars' = map (\t -> foldr applyForall t foralls) t_vars
    t_return' = foldr applyForall t_return foralls

    applyForall fa t | usesTVar tvar t = fa t
                     | otherwise       = t
      where TAll tvar _ = fa t

    (t_vars, t_return) = go [] typ'
    (foralls, typ') = skipForalls typ


    go acc t = case t of
        TFun t1 t2 -> go (snoc t1 acc) t2
        _          -> (acc, t)

usesTVar :: TVar -> Type -> Bool
usesTVar tvar = \case
    TLit _                       -> False
    TVar tvar' | tvar' == tvar   -> True
               | otherwise       -> False
    TFun t1 t2                   -> on (||) usesTVar' t1 t2
    TAll tvar' t | tvar' == tvar -> error "Redeclaration of TVar"
                 | otherwise     -> usesTVar' t
    TData _ typs                 -> any usesTVar' typs
    _                            -> error "Impossible"
  where
    usesTVar' = usesTVar tvar

skipLambdas :: Int -> T.Exp' Type -> T.Exp' Type
skipLambdas i exp
  | i == 0                 = exp
  | T.EAbs _ (e, _) <- exp = skipLambdas (i-1) e
  | otherwise              = error "Number of expected lambdas doesn't match expression"

isComplete :: Env -> Bool
isComplete = isNothing . S.findIndexL unSolvedTEVar
  where
    unSolvedTEVar = \case
        EnvTEVar _ -> True
        _          -> False

toTVar :: Type -> Err TVar
toTVar = \case
    TVar tvar -> pure tvar
    _         -> throwError "Not a type variable"

insertEnv :: EnvElem -> Tc ()
insertEnv x = modifyEnv (:|> x)

lookupBind :: LIdent -> Tc (Maybe Exp)
lookupBind x = gets (Map.lookup x . binds)

lookupDataType :: UIdent -> Tc (Maybe (Type, [(UIdent, Type)]))
lookupDataType x = gets (Map.lookup x . data_types)

lookupSig :: LIdent -> Tc (Maybe Type)
lookupSig x = gets (Map.lookup x . sig)

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
  modify $ \cxt -> trace (ppEnv (f cxt.env)) cxt { env = f cxt.env }

pattern DBind' name vars exp = DBind (Bind name vars exp)
pattern DSig' name typ = DSig (Sig name typ)

dummy = TLit "Int"

---------------------------------------------------------------------------
-- * Debug
---------------------------------------------------------------------------

traceEnv s = do
  env <- gets env
  trace (s ++ " " ++ show env) pure ()

traceD s x = trace (s ++ " " ++ show x) pure ()

traceT s x = trace (s ++ " " ++ ppT x) pure ()

traceTs s xs = trace (s ++ " [ " ++ intercalate ", " (map ppT xs) ++ " ]") pure ()

ppT = \case
    TLit (UIdent s)            -> s
    TVar (MkTVar (LIdent s))   -> "α_" ++ s
    TFun t1 t2                 -> ppT t1 ++ "→" ++ ppT t2
    TAll (MkTVar (LIdent s)) t -> "forall " ++ s ++ ". " ++ ppT t
    TEVar (MkTEVar (LIdent s)) -> "ά_" ++ s
    TData (UIdent name) typs   -> name ++ " (" ++ unwords (map ppT typs)
                                       ++ " )"
ppEnvElem = \case
   EnvVar         (LIdent s) t           -> s ++ ":" ++ ppT t
   EnvTVar        (MkTVar  (LIdent s))   -> "α_" ++ s
   EnvTEVar       (MkTEVar (LIdent s))   -> "ά_" ++ s
   EnvTEVarSolved (MkTEVar (LIdent s)) t -> "ά_" ++ s ++ "=" ++ ppT t
   EnvMark        (MkTEVar (LIdent s))   -> "▶" ++ "ά_" ++ s

ppEnv = \case
    Empty      -> "·"
    (xs :|> x) -> ppEnv xs ++ " (" ++ ppEnvElem x ++ ")"
