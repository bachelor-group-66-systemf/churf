{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module LambdaLifter (lambdaLift, freeVars, abstract, rename, collectScs) where

import           Auxiliary           (snoc)
import           Control.Applicative (Applicative (liftA2))
import           Control.Monad.State (MonadState (get, put), State, evalState)
import           Data.Foldable.Extra (notNull)
import           Data.List           (mapAccumL, partition)
import           Data.Set            (Set, (\\))
import qualified Data.Set            as Set
import           Prelude             hiding (exp)
import           Renamer             hiding (fromBinders)
import           TypeCheckerIr


-- | Lift lambdas and let expression into supercombinators.
lambdaLift :: Program -> Program
lambdaLift = collectScs . abstract . freeVars


-- | Annotate free variables
freeVars :: Program -> AnnProgram
freeVars (Program ds) = [ (n, xs, freeVarsExp (Set.fromList xs) e)
                        | Bind n xs e <- ds
                        ]

freeVarsExp :: Set Id -> Exp -> AnnExp
freeVarsExp localVars = \case

  EId  n  | Set.member n localVars -> (Set.singleton n, AId n)
          | otherwise              -> (mempty, AId n)

  EInt i -> (mempty, AInt i)

  EApp t e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AApp t e1' e2')
    where
      e1' = freeVarsExp localVars e1
      e2' = freeVarsExp localVars e2

  EAdd t e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AAdd t e1' e2')
    where
      e1' = freeVarsExp localVars e1
      e2' = freeVarsExp localVars e2

  EAbs t par e -> (Set.delete par $ freeVarsOf e', AAbs t par e')
    where
      e' = freeVarsExp (Set.insert par localVars) e

  -- Sum free variables present in binders and the expression
  ELet binders e  -> (Set.union binders_frees e_free, ALet binders' e')
    where
      binders_frees = rhss_frees \\ names_set
      e_free        = freeVarsOf e' \\ names_set

      rhss_frees = foldr1 Set.union (map freeVarsOf rhss')
      names_set  = Set.fromList names

      (names, parms, rhss) = fromBinders binders
      rhss'                = map (freeVarsExp e_localVars) rhss
      e_localVars          = Set.union localVars names_set

      binders' = zipWith3 ABind names parms rhss'
      e'       = freeVarsExp e_localVars e

  EAnn e t -> (freeVarsOf e', AAnn e' t)
    where
      e' = freeVarsExp localVars e


freeVarsOf :: AnnExp -> Set Id
freeVarsOf = fst


fromBinders :: [Bind] -> ([Id], [[Id]], [Exp])
fromBinders bs = unzip3 [ (name, parms, rhs) | Bind name parms rhs <- bs ]


-- AST annotated with free variables
type AnnProgram = [(Id, [Id], AnnExp)]

type AnnExp = (Set Id, AnnExp')

data ABind = ABind Id [Id] AnnExp deriving Show

data AnnExp' = AId  Id
             | AInt Integer
             | ALet [ABind] AnnExp
             | AApp Type    AnnExp  AnnExp
             | AAdd Type    AnnExp  AnnExp
             | AAbs Type    Id      AnnExp
             | AAnn AnnExp  Type
             deriving Show


-- | Lift lambdas to let expression of the form @let sc = \v₁ x₁ -> e₁@.
-- Free variables are @v₁ v₂ .. vₙ@ are bound.
abstract :: AnnProgram -> Program
abstract prog = Program $ evalState (mapM go prog) 0
  where
  go :: (Id, [Id], AnnExp) -> State Int Bind
  go (name, parms, rhs) = Bind name (parms ++ parms1) <$> abstractExp rhs'
    where
      (rhs', parms1) = flattenLambdasAnn rhs


-- | Flatten nested lambdas and collect the parameters
-- @\x.\y.\z. ae → (ae, [x,y,z])@
flattenLambdasAnn :: AnnExp -> (AnnExp, [Id])
flattenLambdasAnn ae = go (ae, [])
  where
    go :: (AnnExp, [Id]) -> (AnnExp, [Id])
    go ((free, e), acc) =
      case e of
        AAbs _ par (free1, e1) ->
          go ((Set.delete par free1, e1), snoc par acc)
        _ -> ((free, e), acc)

abstractExp :: AnnExp -> State Int Exp
abstractExp (free, exp) = case exp of
  AId  n       -> pure $ EId  n
  AInt i       -> pure $ EInt i
  AApp t e1 e2 -> liftA2 (EApp t) (abstractExp e1) (abstractExp e2)
  AAdd t e1 e2 -> liftA2 (EAdd t) (abstractExp e1) (abstractExp e2)
  ALet bs e    -> liftA2 ELet (mapM go bs) (abstractExp e)
    where
      go (ABind name parms rhs) = do
          (rhs', parms1)  <- flattenLambdas <$> skipLambdas abstractExp rhs
          pure $ Bind name (parms ++ parms1) rhs'

      skipLambdas :: (AnnExp -> State Int Exp) -> AnnExp -> State Int Exp
      skipLambdas f (free, ae) = case ae of
        AAbs t par ae1 -> EAbs t par <$> skipLambdas f ae1
        _              -> f (free, ae)

  -- Lift lambda into let and bind free variables
  AAbs t parm e -> do
    i <- nextNumber
    rhs <- abstractExp e

    let sc_name = Ident ("sc_" ++ show i)
        sc      = ELet [Bind (sc_name, t) parms rhs] $ EId (sc_name, t)

    pure $ foldl (EApp TInt) sc $ map EId freeList
    where
      freeList = Set.toList free
      parms    = snoc parm freeList

  AAnn e t -> abstractExp e >>= \e' -> pure $ EAnn e' t

nextNumber :: State Int Int
nextNumber = do
  i <- get
  put $ succ i
  pure i

-- | Collects supercombinators by lifting appropriate let expressions
collectScs :: Program -> Program
collectScs (Program scs) = Program $ concatMap collectFromRhs scs
  where
    collectFromRhs (Bind name parms rhs) =
      let (rhs_scs, rhs') = collectScsExp rhs
      in  Bind name parms rhs' : rhs_scs


collectScsExp :: Exp -> ([Bind], Exp)
collectScsExp = \case
  EId  n -> ([], EId n)
  EInt i -> ([], EInt i)

  EApp t e1 e2 -> (scs1 ++ scs2, EApp t e1' e2')
    where
      (scs1, e1') = collectScsExp e1
      (scs2, e2') = collectScsExp e2

  EAdd t e1 e2 -> (scs1 ++ scs2, EAdd t e1' e2')
    where
      (scs1, e1') = collectScsExp e1
      (scs2, e2') = collectScsExp e2

  EAbs t par e -> (scs, EAbs t par e')
    where
      (scs, e') = collectScsExp e

  -- Collect supercombinators from binds, the rhss, and the expression.
  --
  -- > f = let
  -- >       sc  = rhs
  -- >       sc1 = rhs1
  -- >          ...
  -- >     in e
  --
  ELet binds e -> (binds_scs ++ rhss_scs ++ e_scs, mkEAbs non_scs' e')
    where
      binds_scs          = [ let (rhs', parms1) = flattenLambdas rhs in
                             Bind n (parms ++ parms1) rhs'
                           | Bind n parms rhs <- scs'
                           ]
      (rhss_scs, binds') = mapAccumL collectScsRhs [] binds
      (e_scs, e')        = collectScsExp e

      (scs', non_scs') = partition (\(Bind _ pars _) -> notNull pars) binds'

      collectScsRhs acc (Bind n xs rhs) = (acc ++ rhs_scs, Bind n xs rhs')
        where
          (rhs_scs, rhs') = collectScsExp rhs

  EAnn e t -> (scs, EAnn e' t)
    where
      (scs, e') = collectScsExp e

-- @\x.\y.\z. e → (e, [x,y,z])@
flattenLambdas :: Exp -> (Exp, [Id])
flattenLambdas = go . (, [])
  where
    go (e, acc) = case e of
                   EAbs _ par e1 -> go (e1, snoc par acc)
                   _             -> (e, acc)

mkEAbs :: [Bind] -> Exp -> Exp
mkEAbs [] e = e
mkEAbs bs e = ELet bs e
