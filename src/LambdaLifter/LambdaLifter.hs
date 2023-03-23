{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaLifter.LambdaLifter where

import Auxiliary (snoc)
import Control.Applicative (Applicative (liftA2))
import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.Set (Set)
import Data.Set qualified as Set
import Renamer.Renamer
import TypeChecker.TypeChecker (partitionType)
import TypeChecker.TypeCheckerIr
import Prelude hiding (exp)

{- | Lift lambdas and let expression into supercombinators.
Three phases:
@freeVars@ annotates all the free variables.
@abstract@ converts lambdas into let expressions.
@collectScs@ moves every non-constant let expression to a top-level function.
-}
lambdaLift :: Program -> Program
lambdaLift = collectScs . abstract . freeVars

-- | Annotate free variables
freeVars :: Program -> AnnProgram
freeVars (Program ds) =
    [ (n, xs, freeVarsExp (Set.fromList $ map fst xs) e)
    | Bind n xs e <- ds
    ]

freeVarsExp :: Set Ident -> ExpT -> AnnExpT
freeVarsExp localVars (exp, t) = case exp of
    EId n
        | Set.member n localVars -> (Set.singleton n, (AId n, t))
        | otherwise -> (mempty, (AId n, t))
    -- EInt i -> (mempty, AInt i)
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
    ELet (Bind (name, t_bind) parms rhs) e -> (Set.union binders_frees e_free, (ALet new_bind e', t))
      where
        binders_frees = Set.delete name $ freeVarsOf rhs'
        e_free = Set.delete name $ freeVarsOf e'

        rhs' = freeVarsExp e_localVars rhs
        new_bind = ABind (name, t_bind) parms rhs'

        e' = freeVarsExp e_localVars e
        e_localVars = Set.insert name localVars

freeVarsOf :: AnnExpT -> Set Ident
freeVarsOf = fst

-- AST annotated with free variables
type AnnProgram = [(Id, [Id], AnnExpT)]

type AnnExpT = (Set Ident, AnnExpT')

data ABind = ABind Id [Id] AnnExpT deriving (Show)

type AnnExpT' = (AnnExp, Type)

data AnnExp
    = AId Ident
    | ALit Lit
    | ALet ABind AnnExpT
    | AApp AnnExpT AnnExpT
    | AAdd AnnExpT AnnExpT
    | AAbs Ident AnnExpT
    deriving (Show)

{- | Lift lambdas to let expression of the form @let sc = \v₁ x₁ -> e₁@.
Free variables are @v₁ v₂ .. vₙ@ are bound.
-}
abstract :: AnnProgram -> Program
abstract prog = Program $ evalState (mapM go prog) 0
  where
    go :: (Id, [Id], AnnExpT) -> State Int Bind
    go (name, parms, rhs) = Bind name (parms ++ parms1) <$> abstractExp rhs'
      where
        (rhs', parms1) = flattenLambdasAnn rhs

{- | Flatten nested lambdas and collect the parameters
@\x.\y.\z. ae → (ae, [x,y,z])@
-}
flattenLambdasAnn :: AnnExpT -> (AnnExpT, [Id])
flattenLambdasAnn ae = go (ae, [])
  where
    go :: (AnnExpT, [Id]) -> (AnnExpT, [Id])
    go ((free, (e, t)), acc)
        | AAbs par (free1, e1) <- e
        , TFun t_par _ <- t =
            go ((Set.delete par free1, e1), snoc (par, t_par) acc)
        | otherwise = ((free, (e, t)), acc)

abstractExp :: AnnExpT -> State Int ExpT
abstractExp (free, (exp, t)) = case exp of
    AId n -> pure (EId n, t)
    ALit lit -> pure (ELit lit, t)
    AApp e1 e2 -> (,t) <$> liftA2 EApp (abstractExp e1) (abstractExp e2)
    AAdd e1 e2 -> (,t) <$> liftA2 EAdd (abstractExp e1) (abstractExp e2)
    ALet b e -> (,t) <$> liftA2 ELet (go b) (abstractExp e)
      where
        go (ABind name parms rhs) = do
            (rhs', parms1) <- flattenLambdas <$> skipLambdas abstractExp rhs
            pure $ Bind name (parms ++ parms1) rhs'

        skipLambdas :: (AnnExpT -> State Int ExpT) -> AnnExpT -> State Int ExpT
        skipLambdas f (free, (ae, t)) = case ae of
            AAbs par ae1 -> do
                ae1' <- skipLambdas f ae1
                pure (EAbs par ae1', t)
            _ -> f (free, (ae, t))

    -- Lift lambda into let and bind free variables
    AAbs parm e -> do
        i <- nextNumber
        rhs <- abstractExp e

        let sc_name = Ident ("sc_" ++ show i)
            sc = (ELet (Bind (sc_name, t) vars rhs) (EId sc_name, t), t)
        pure $ foldl applyVars sc freeList
      where
        freeList = Set.toList free
        vars = zip names . fst $ partitionType (length names) t
        names = snoc parm freeList
        applyVars (e, t) name = (EApp (e, t) (EId name, t_var), t_return)
          where
            (t_var : _, t_return) = partitionType 1 t

nextNumber :: State Int Int
nextNumber = do
    i <- get
    put $ succ i
    pure i

-- | Collects supercombinators by lifting non-constant let expressions
collectScs :: Program -> Program
collectScs (Program scs) = Program $ concatMap collectFromRhs scs
  where
    collectFromRhs (Bind name parms rhs) =
        let (rhs_scs, rhs') = collectScsExp rhs
         in Bind name parms rhs' : rhs_scs

collectScsExp :: ExpT -> ([Bind], ExpT)
collectScsExp expT@(exp, typ) = case exp of
    EId _ -> ([], expT)
    ELit _ -> ([], expT)
    EApp e1 e2 -> (scs1 ++ scs2, (EApp e1' e2', typ))
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

    -- Collect supercombinators from bind, the rhss, and the expression.
    --
    -- > f = let sc x y = rhs in e
    --
    ELet (Bind name parms rhs) e ->
        if null parms
            then (rhs_scs ++ et_scs, (ELet bind et', snd et'))
            else (bind : rhs_scs ++ et_scs, et')
      where
        bind = Bind name parms rhs'
        (rhs_scs, rhs') = collectScsExp rhs
        (et_scs, et') = collectScsExp e

-- @\x.\y.\z. e → (e, [x,y,z])@
flattenLambdas :: ExpT -> (ExpT, [Id])
flattenLambdas = go . (,[])
  where
    go ((e, t), acc) = case e of
        EAbs name e1 -> go (e1, snoc (name, t_var) acc)
          where
            t_var : _ = fst $ partitionType 1 t
        _ -> ((e, t), acc)
