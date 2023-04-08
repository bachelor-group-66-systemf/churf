{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeChecker.TypeCheckerBidir (typecheck, getVars) where

import           Auxiliary                 (int, litType, maybeToRightM, snoc)
import           Control.Applicative       (Alternative, Applicative (liftA2),
                                            (<|>))
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            liftEither, runExceptT, unless,
                                            zipWithM, zipWithM_)
import           Control.Monad.State       (MonadState, State, evalState, gets,
                                            modify)
import           Data.Coerce               (coerce)
import           Data.Foldable             (foldrM)
import           Data.Function             (on)
import           Data.List                 (intercalate, partition)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isNothing)
import           Data.Sequence             (Seq (..))
import qualified Data.Sequence             as S
import qualified Data.Set                  as Set
import           Data.Tuple.Extra          (second, secondM)
import           Debug.Trace               (trace)
import           Grammar.Abs
import           Grammar.ErrM
import           Grammar.Print             (printTree)
import           Prelude                   hiding (exp, id)
import qualified TypeChecker.TypeCheckerIr as T

-- Implementation is derived from the paper (Dunfield and Krishnaswami 2013)
-- https://doi.org/10.1145/2500365.2500582
--
-- TODO
-- • Fix problems with types in Pattern/Branch in TypeCheckerIr
-- • Use applyEnvExp consistently
-- • Fix the different type getters functions (e.g. partitionType) functions

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
    { env        :: Env             -- ^ Local scope context  Γ
    , sig        :: Map LIdent Type -- ^ Top-level signatures x : A
    , binds      :: Map LIdent Exp  -- ^ Top-level binds x : e
    , next_tevar :: Int             -- ^ Counter to distinguish ά
    , data_injs  :: Map UIdent Type -- ^ Data injections (constructors) K/inj : A
    } deriving (Show, Eq)

newtype Tc a = Tc { runTc :: ExceptT String (State Cxt) a }
    deriving (Functor, Applicative, Monad, Alternative, MonadState Cxt, MonadError String)

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
    bind'@(T.Bind (name, typ) _ _) <- lookupSig name >>= \case
        Just t  -> do
            (rhs', _) <- check (foldr EAbs rhs vars) t
            pure (T.Bind (coerce name, t) [] (rhs', t))
        Nothing -> do
            (e, t) <- infer $ foldr EAbs rhs vars
            t'     <- applyEnv t
            e'     <- applyEnvExp e
            pure (T.Bind (coerce name, t') [] (e', t'))
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

-- TODO remove some checks
typecheckInj :: Inj -> UIdent -> [TVar] -> Err (T.Inj' Type)
typecheckInj (Inj inj_name inj_typ) name tvars
    | not $ boundTVars tvars inj_typ
    = throwError "Unbound type variables"
    | TData name' typs <- getReturn inj_typ
    , name'  == name
    , Right tvars' <- mapM toTVar typs
    , all (`elem` tvars) tvars'
    = pure $ T.Inj (coerce inj_name) (foldr TAll inj_typ tvars')
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
        insertEnv env_marker
        insertEnv $ EnvTEVar tevar
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
        | noForall typ
        , (env_l, env_r) <- splitOn (EnvTEVar tevar) env
        , Right _ <-  wellFormed env_l typ
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
        | noForall typ
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
        let env_var = EnvVar name t1
        insertEnv env_var
        e' <- check e t2
        (env_l, _) <- gets (splitOn env_var . env)
        putEnv env_l
        pure (T.EAbs (coerce name) e', typ)

    | otherwise = subsumption
  where
    --  Γ,α ⊢ e ↓ A ⊣ Θ   Θ ⊢ [Θ]A <: [Θ]B ⊣ Δ
    --  -------------------------------------- Sub
    --  Γ ⊢ e ↑ B ⊣ Δ
    subsumption = do
      (exp', t) <- infer exp
      exp'' <- applyEnvExp exp'
      t' <- applyEnv t
      typ' <- applyEnv typ
      subtype t' typ'
      pure (exp'', t')

-- | Γ ⊢ e ↓ A ⊣ Δ
-- Under input context Γ, e infers output type A, with output context ∆
infer :: Exp -> Tc (T.ExpT' Type)
infer = \case

    ELit lit -> pure (T.ELit lit, litType lit)

    --  (x : A) ∈ Γ         (x : A) ∉ Γ
    --  ------------- Var   --------------- Var'
    --  Γ ⊢ x ↓ A ⊣ Γ       Γ ⊢ x ↓ ά ⊣ Γ,ά
    EVar name -> do
        t <- liftA2 (<|>) (lookupEnv name) (lookupSig name) >>= \case
                 Just t  -> pure t
                 Nothing -> do
                     tevar <- fresh
                     insertEnv (EnvTEVar tevar)
                     let t = TEVar tevar
                     insertEnv (EnvVar name t)
                     pure t
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
      let env_var = EnvVar name (TEVar tevar1)
      insertEnv env_var
      e' <- check e $ TEVar tevar2
      dropTrailing env_var
      let t_exp = on TFun TEVar tevar1 tevar2
      pure (T.EAbs (coerce name) e', t_exp)


    --  Γ ⊢ e ↓ A ⊣ Θ   Θ,(x:A) ⊢ e' ↑ C ⊣ Δ,(x:A),Θ
    --  -------------------------------------------- LetI
    --  Γ ⊢ let x=e in e' ↑ C ⊣ Δ
    ELet (Bind name [] rhs) e -> do -- TODO vars
        (rhs', t_rhs) <- infer rhs
        let env_var = EnvVar name t_rhs
        insertEnv env_var
        (e', t) <- infer e
        (env_l, _) <- gets (splitOn env_var . env)
        putEnv env_l
        pure (T.ELet (T.Bind (coerce name, t_rhs) [] (rhs', t_rhs)) (e',t), t)

    --  Γ ⊢ e₁ ↑ Int ⊣ Θ  Θ ⊢ e₂ ↑ Int
    --  --------------------------- +I
    --  Γ ⊢ e₁ + e₂ ↓ Int ⊣ Δ
    EAdd e1 e2 -> do
        e1' <- check e1 int
        e2' <- check e2 int
        e1'' <- applyEnvExpT e1'
        e2'' <- applyEnvExpT e2'
        pure (T.EAdd e1'' e2'', int)

    --                  Θ ⊢ Π ∷ A ↓ C ⊣ Δ
    --  Γ ⊢ e ↓ A ⊣ Θ   Δ ⊢ Π covers [Δ]A TODO
    --  ---------------------------------------
    --  Γ ⊢ case e of Π ↓ C ⊣ Δ
    ECase scrut branches -> do
      (scrut', t_scrut) <- infer scrut
      (branches', t_return) <- inferBranches branches t_scrut
      pure (T.ECase (scrut', t_scrut) branches', t_return)

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

--  Γ ⊢ p ⇒ e ∷ A ↓ B ⊣ Θ
--  Θ ⊢ Π ∷ [Θ]A ↓ C ⊣ Δ
--  [Δ]B <: C
--  ---------------------------
--  Γ  ⊢ (p ⇒ e),Π ∷ A ↓ C ⊣ Δ
inferBranches :: [Branch] -> Type -> Tc ([T.Branch' Type], Type)
inferBranches branches t_patt = do
    (branches', ts_exp) <- inferBranches' t_patt branches
    ts_exp' <- mapM applyEnv ts_exp
    let (monos, pols) = partition isMono ts_exp'
    t_exp <- liftEither $ bodyType t_patt monos
    mapM_ (subtype t_exp) pols
    pure (branches', t_exp)
  where

    bodyType :: Type -> [Type] -> Err Type
    bodyType t_patt = \case
        []     -> pure t_patt
        [m]    -> pure m
        m:n:ms | m == n    -> bodyType t_patt (n:ms)
               | otherwise -> throwError $ unwords [ "Wrong return types: "
                                                   , ppT m, "≠", ppT n ]

    inferBranches' = go [] []
      where
        go branches ts_exp t = \case
            [] -> pure (branches, ts_exp)
            b:bs -> do
              (b', t_e) <- inferBranch b t
              t' <- applyEnv t
              go (snoc b' branches) (snoc t_e ts_exp) t' bs

--  Γ ⊢ p ↑ A ⊣ Θ  Θ ⊢ e ↓ C ⊣ Δ
--  -------------------------------
--  Γ ⊢ p ⇒ e ∷ A ↓ C ⊣ Δ
inferBranch :: Branch -> Type -> Tc (T.Branch' Type, Type)
inferBranch (Branch patt exp) t_patt = do
    patt' <- checkPattern patt t_patt
    (exp', t_exp) <- infer exp
    pure (T.Branch patt' (exp', t_exp), t_exp)

checkPattern :: Pattern -> Type -> Tc (T.Pattern' Type, Type)
checkPattern patt t_patt = case patt of

        --  -------------------
        --  Γ ⊢ x ↑ A ⊣ Γ,(x:A)
        PVar x -> do
            insertEnv $ EnvVar x t_patt
            pure (T.PVar (coerce x, t_patt), t_patt)

        --  -------------
        --  Γ ⊢ _ ↑ A ⊣ Γ
        PCatch -> pure (T.PCatch, t_patt)

        --  Γ ⊢ τ ↓ A ⊣ Γ   Γ ⊢ A <: B ⊣ Δ
        --  ------------------------------
        --  Γ ⊢ τ ↑ B ⊣ Δ
        PLit lit -> do
          subtype (litType lit) t_patt
          t_patt' <- applyEnv t_patt
          pure (T.PLit (lit, t_patt), t_patt')

        --  (x : A) ∈ Γ  Γ ⊢ A <: B ⊣ Δ
        --  ---------------------------
        --  Γ ⊢ inj₀ x ↑ B ⊣ Δ
        PEnum name -> do
            t <- maybeToRightM ("Unknown constructor " ++ show name)
                     =<< lookupInj name
            subtype t t_patt
            t_patt' <- applyEnv t_patt
            pure (T.PEnum (coerce name), t_patt')


        PInj name ps -> do
            t_inj <- maybeToRightM "unknown constructor" =<< lookupInj name
            t_inj' <- foldrM substitute' t_inj $ getInitForalls t_inj
            subtype (getDataId t_inj') t_patt
            t_inj'' <- applyEnv t_inj'
            let ts_inj = getParams t_inj''
            ps' <- zipWithM (\p t -> checkPattern p =<< applyEnv t) ps ts_inj
            t_patt' <- applyEnv t_patt
            pure (T.PInj (coerce name) (map fst ps'), t_patt')
          where
            substitute' fa t = do
                tevar <- fresh
                -- insertEnv (EnvTEVar tevar)
                pure $ substitute tvar tevar t
              where
                TAll tvar _ = fa int

            getParams = \case
                TAll _ t -> getParams t
                t        -> go [] t
              where
                go acc = \case
                  TFun t1 t2 -> go (snoc t1 acc) t2
                  _          -> acc

            getDataId typ = case typ of
              TAll _ t -> getDataId t
              TFun _ t -> getDataId t
              TData {} -> typ

            getInitForalls = go []
              where
                go acc = \case
                  TAll tvar t -> go (snoc (TAll tvar) acc) t
                  _           -> acc

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

applyEnvExpT :: (T.Exp' Type, Type) -> Tc (T.Exp' Type, Type)
applyEnvExpT (e, t) = liftA2 (,) (applyEnvExp e) (applyEnv t)

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

noForall :: Type -> Bool
noForall = \case
  TAll{}       -> False
  TFun t1 t2   -> on (&&) noForall t1 t2
  TData _ typs -> all noForall typs
  TVar _       -> True
  TEVar _      -> True
  TLit _       -> True

isMono :: Type -> Bool
isMono = \case
  TAll{}       -> False
  TFun t1 t2   -> on (&&) isMono t1 t2
  TData _ typs -> all isMono typs
  TVar _       -> False
  TEVar _      -> False
  TLit _       -> True

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
-- * Debug
---------------------------------------------------------------------------

traceEnv s = do
  env <- gets env
  trace (s ++ " " ++ ppEnv env) pure ()

traceD s x = trace (s ++ " " ++ show x) pure ()

traceT s x = trace (s ++ " " ++ ppT x) pure ()

traceTs s xs = trace (s ++ " [ " ++ intercalate ", " (map ppT xs) ++ " ]") pure ()

ppT = \case
    TLit (UIdent s)            -> s
    TVar (MkTVar (LIdent s))   -> "a_" ++ s
    TFun t1 t2                 -> ppT t1 ++ "->" ++ ppT t2
    TAll (MkTVar (LIdent s)) t -> "forall " ++ s ++ ". " ++ ppT t
    TEVar (MkTEVar (LIdent s)) -> "a^_" ++ s
    TData (UIdent name) typs   -> name ++ " (" ++ unwords (map ppT typs)
                                       ++ " )"
ppEnvElem = \case
   EnvVar         (LIdent s) t           -> s ++ ":" ++ ppT t
   EnvTVar        (MkTVar  (LIdent s))   -> "a_" ++ s
   EnvTEVar       (MkTEVar (LIdent s))   -> "a^_" ++ s
   EnvTEVarSolved (MkTEVar (LIdent s)) t -> "_" ++ s ++ "=" ++ ppT t
   EnvMark        (MkTEVar (LIdent s))   -> "▶" ++ "a^_" ++ s

ppEnv = \case
    Empty      -> "·"
    (xs :|> x) -> ppEnv xs ++ " (" ++ ppEnvElem x ++ ")"
