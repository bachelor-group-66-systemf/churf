{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module LambdaLifter (lambdaLift, freeVars, abstract, collectScs) where

import           Auxiliary                 (mapAccumM, snoc)
import           Control.Applicative       (Applicative (liftA2))
import           Control.Arrow             (Arrow (second))
import           Control.Monad.State       (MonadState (get, put), State,
                                            evalState)
import           Data.List                 (mapAccumL, partition)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Prelude                   hiding (exp)
import           TypeChecker.TypeCheckerIr


-- | Lift lambdas and let expression into supercombinators.
-- Three phases:
-- @freeVars@ annotates all the free variables.
-- @abstract@ converts lambdas into let expressions.
-- @collectScs@ moves every non-constant let expression to a top-level function.
lambdaLift :: Program -> Program
lambdaLift (Program defs) = Program $ datatypes ++ ll binds
  where
    ll = map DBind . collectScs . abstract . freeVars . map (\(DBind b) -> b)
    (binds, datatypes) = partition isBind defs
    isBind = \case
      DBind _ -> True
      _       -> False

-- | Annotate free variables
freeVars :: [Bind] -> AnnBinds
freeVars binds = [ (n, xs, freeVarsExp (Set.fromList $ map fst xs) e)
                 | Bind n xs e <- binds
                 ]

freeVarsExp :: Set Ident -> ExpT -> AnnExpT
freeVarsExp localVars (exp, t) = case exp of
    EVar  n  | Set.member n localVars -> (Set.singleton n, (AVar n, t))
             | otherwise              -> (mempty, (AVar n, t))

    EInj n -> (mempty, (AVar n, t))

    ELit lit -> (mempty, (ALit lit, t))

    EApp e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), (AApp e1' e2', t))
      where
        e1' = freeVarsExp localVars e1
        e2' = freeVarsExp localVars e2

    EAdd e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), (AAdd e1' e2', t))
      where
        e1' = freeVarsExp localVars e1
        e2' = freeVarsExp localVars e2

    EAbs par e -> (Set.delete par $ freeVarsOf e', (AAbs par e', t))
      where
        e' = freeVarsExp (Set.insert par localVars) e

    -- Sum free variables present in bind and the expression
    ELet (Bind (name, t_bind) parms rhs) e  -> (Set.union binders_frees e_free, (ALet new_bind e', t))
      where
        binders_frees = Set.delete name $ freeVarsOf rhs'
        e_free        = Set.delete name $ freeVarsOf e'

        rhs'     = freeVarsExp e_localVars rhs
        new_bind = ABind (name, t_bind) parms rhs'

        e'          = freeVarsExp e_localVars e
        e_localVars = Set.insert name localVars

    ECase e branches -> (frees, (ACase e' branches', t))
      where
        frees = foldr (\b s -> Set.union s $ fst b) (freeVarsOf  e') branches'
        e' = freeVarsExp localVars e
        branches' = map (freeVarsBranch localVars) branches


freeVarsBranch :: Set Ident -> Branch' Type -> (Set Ident, AnnBranch')
freeVarsBranch localVars (Branch (patt, t) exp) = (frees, AnnBranch (patt, t) exp')
  where
    frees = freeVarsOf exp' Set.\\ freeVarsOfPattern patt
    exp' = freeVarsExp localVars exp
    freeVarsOfPattern = Set.fromList . go []
      where
        go acc = \case
            PVar (n,_) -> snoc n acc
            PInj _ ps  ->  foldl go acc ps



freeVarsOf :: AnnExpT -> Set Ident
freeVarsOf = fst

-- AST annotated with free variables
type AnnBinds = [(Id, [Id], AnnExpT)]

type AnnExpT = (Set Ident, AnnExpT')

data ABind = ABind Id [Id] AnnExpT deriving Show

type AnnExpT' = (AnnExp, Type)

type AnnBranch = (Set Ident, AnnBranch')
data AnnBranch' = AnnBranch (Pattern, Type) AnnExpT
  deriving Show

data AnnExp = AVar Ident
            | AInj Ident
            | ALit Lit
            | ALet ABind    AnnExpT
            | AApp AnnExpT  AnnExpT
            | AAdd AnnExpT  AnnExpT
            | AAbs Ident    AnnExpT
            | ACase AnnExpT [AnnBranch]
              deriving Show

-- | Lift lambdas to let expression of the form @let sc = \v₁ x₁ -> e₁@.
-- Free variables are @v₁ v₂ .. vₙ@ are bound.
abstract :: AnnBinds -> [Bind]
abstract prog = evalState (mapM go prog) 0
  where
    go :: (Id, [Id], AnnExpT) -> State Int Bind
    go (name, parms, rhs) = Bind name (parms ++ parms1) <$> abstractExp rhs'
      where
        (rhs', parms1) = flattenLambdasAnn rhs


-- | Flatten nested lambdas and collect the parameters
-- @\x.\y.\z. ae → (ae, [x,y,z])@
flattenLambdasAnn :: AnnExpT -> (AnnExpT, [Id])
flattenLambdasAnn ae = go (ae, [])
  where
    go :: (AnnExpT, [Id]) -> (AnnExpT, [Id])
    go ((free, (e, t)), acc)
        | AAbs par (free1, e1) <- e
        , TFun t_par _ <- t
        = go ((Set.delete par free1, e1), snoc (par, t_par) acc)
        | otherwise = ((free, (e, t)), acc)

abstractExp :: AnnExpT -> State Int ExpT
abstractExp (free, (exp, typ)) = case exp of
    AVar  n    -> pure (EVar  n, typ)
    AInj  n    -> pure (EInj  n, typ)
    ALit lit   -> pure (ELit lit, typ)
    AApp e1 e2 -> (, typ) <$> liftA2 EApp (abstractExp e1) (abstractExp e2)
    AAdd e1 e2 -> (, typ) <$> liftA2 EAdd (abstractExp e1) (abstractExp e2)
    ALet b e   -> (, typ) <$> liftA2 ELet (go b) (abstractExp e)
      where
        go (ABind name parms rhs) = do
            (rhs', parms1)  <- flattenLambdas <$> skipLambdas abstractExp rhs
            pure $ Bind name (parms ++ parms1) rhs'

        skipLambdas :: (AnnExpT -> State Int ExpT) -> AnnExpT -> State Int ExpT
        skipLambdas f (free, (ae, t)) = case ae of
            AAbs par ae1 -> do
                ae1' <- skipLambdas f ae1
                pure (EAbs par ae1', t)
            _            -> f (free, (ae, t))

    ACase e branches -> (, typ) <$> liftA2 ECase (abstractExp e) (mapM abstractBranch branches)


    -- Lift lambda into let and bind free variables
    AAbs parm e -> do
        i <- nextNumber
        rhs <- abstractExp e

        let sc_name = Ident ("sc_" ++ show i)
            sc      = (ELet (Bind (sc_name, typ) vars rhs) (EVar sc_name, typ), typ)
        pure $ foldl applyVars sc freeList

      where
        freeList = Set.toList free
        vars = zip names $ getVars typ
        names = snoc parm freeList
        applyVars (e, t) name = (EApp (e, t) (EVar name, t_var), t_return)
          where
            (t_var, t_return) = applyVarType t


abstractBranch :: AnnBranch -> State Int Branch
abstractBranch (_, AnnBranch patt exp) = Branch patt <$> abstractExp exp

applyVarType :: Type -> (Type, Type)
applyVarType typ = (t1, foldr ($) t2 foralls)

  where
    (t1, t2) = case typ' of
      TFun t1 t2 -> (t1, t2)
      _          -> error "Not a function!"

    (foralls, typ') = skipForalls [] typ


    skipForalls acc = \case
      TAll tvar t -> skipForalls (snoc (TAll tvar) acc) t
      t           -> (acc, t)

nextNumber :: State Int Int
nextNumber = do
    i <- get
    put $ succ i
    pure i

-- | Collects supercombinators by lifting non-constant let expressions
collectScs :: [Bind] -> [Bind]
collectScs = concatMap collectFromRhs
  where
    collectFromRhs (Bind name parms rhs) =
        let (rhs_scs, rhs') = collectScsExp rhs
        in  Bind name parms rhs' : rhs_scs


collectScsExp :: ExpT -> ([Bind], ExpT)
collectScsExp expT@(exp, typ) = case exp of
    EVar  _ -> ([], expT)
    EInj  _ -> ([], expT)
    ELit _  -> ([], expT)

    EApp e1 e2 -> (scs1 ++ scs2, (EApp  e1' e2', typ))
      where
        (scs1, e1') = collectScsExp e1
        (scs2, e2') = collectScsExp e2

    EAdd e1 e2 -> (scs1 ++ scs2, (EAdd e1' e2', typ))
      where
        (scs1, e1') = collectScsExp e1
        (scs2, e2') = collectScsExp e2

    EAbs par e -> (scs, (EAbs par e', typ))
      where
        (scs, e') = collectScsExp e

    ECase e branches -> (scs ++ scs_e, (ECase e' branches', typ))
      where
          (scs, branches') = mapAccumL f [] branches
          (scs_e, e') = collectScsExp e
          f acc b = (acc ++ acc', b')
            where (acc', b') = collectScsBranch b

    -- Collect supercombinators from bind, the rhss, and the expression.
    --
    -- > f = let sc x y = rhs in e
    --
    ELet (Bind name parms rhs) e -> if null parms
                                    then (       rhs_scs ++ et_scs, (ELet bind et', snd et'))
                                    else (bind : rhs_scs ++ et_scs, et')
      where
        bind            = Bind name parms rhs'
        (rhs_scs, rhs') = collectScsExp rhs
        (et_scs, et')     = collectScsExp e

collectScsBranch (Branch patt exp) = (scs, Branch patt exp')
  where (scs, exp') = collectScsExp exp


-- @\x.\y.\z. e → (e, [x,y,z])@
flattenLambdas :: ExpT -> (ExpT, [Id])
flattenLambdas = go . (, [])
  where
    go ((e, t), acc) = case e of
                           EAbs name e1 -> go (e1, snoc (name, t_var) acc)
                             where t_var = head $ getVars t
                           _           -> ((e, t), acc)

getVars :: Type -> [Type]
getVars = fst . partitionType

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

