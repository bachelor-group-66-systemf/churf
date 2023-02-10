{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module LambdaLifter (lambdaLift, freeVars, abstract, rename, collectScs) where

import           Data.List        (mapAccumL, partition)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (fromMaybe)
import           Data.Set         (Set, (\\))
import qualified Data.Set         as Set
import           Data.Tuple.Extra (uncurry3)
import           Grammar.Abs
import           Prelude          hiding (exp)


-- | Lift lambdas and let expression into supercombinators.
lambdaLift :: Program -> Program
lambdaLift = collectScs . rename . abstract . freeVars


-- | Annotate free variables
freeVars :: Program -> AnnProgram
freeVars (Program ds) = [ (n, xs, freeVarsExp (Set.fromList xs) e)
                        | Bind n xs e <- ds
                        ]

freeVarsExp :: Set Ident -> Exp -> AnnExp
freeVarsExp localVars = \case

  EId n | Set.member n localVars -> (Set.singleton n, AId n)
        | otherwise       -> (mempty, AId n)

  EInt i -> (mempty, AInt i)

  EApp e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AApp e1' e2')
    where
      e1' = freeVarsExp localVars e1
      e2' = freeVarsExp localVars e2

  EAdd e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AAdd e1' e2')
    where
      e1' = freeVarsExp localVars e1
      e2' = freeVarsExp localVars e2

  EAbs parms e -> (freeVarsOf e' \\ Set.fromList parms, AAbs parms e')
    where
      e' = freeVarsExp (foldr Set.insert localVars parms) e

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


freeVarsOf :: AnnExp -> Set Ident
freeVarsOf = fst

fromBinders :: [Bind] -> ([Ident], [[Ident]], [Exp])
fromBinders bs = unzip3 [ (name, parms, rhs) | Bind name parms rhs <- bs ]

-- AST annotated with free variables
type AnnProgram = [(Ident, [Ident], AnnExp)]

type AnnExp = (Set Ident, AnnExp')

data ABind = ABind Ident [Ident] AnnExp deriving Show

data AnnExp' = AId Ident
             | AInt Integer
             | AApp AnnExp  AnnExp
             | AAdd AnnExp  AnnExp
             | AAbs [Ident]   AnnExp
             | ALet [ABind] AnnExp
             deriving Show

-- | Lift lambdas to let expression of the form @let sc = \v₁ x₁ -> e₁@.
-- Free variables are @v₁ v₂ .. vₙ@ are bound.
abstract :: AnnProgram -> Program
abstract prog = Program $ map go prog
  where
  go :: (Ident, [Ident], AnnExp) -> Bind
  go (name, pars, rhs@(_, e)) =

    case e of
      AAbs pars1 e1 -> Bind name (pars ++ pars1) $ abstractExp e1
      _             -> Bind name pars            $ abstractExp rhs

abstractExp :: AnnExp -> Exp
abstractExp (free, exp) = case exp of
  AId  n     -> EId n
  AInt i     -> EInt i
  AApp e1 e2 -> EApp (abstractExp e1) (abstractExp e2)
  AAdd e1 e2 -> EAdd (abstractExp e1) (abstractExp e2)
  ALet bs e  -> ELet (map go bs) $ abstractExp e
    where
      go (ABind name parms rhs) = Bind name parms $ skipLambdas abstractExp rhs

      skipLambdas :: (AnnExp -> Exp) -> AnnExp -> Exp
      skipLambdas f (free, ae) = case ae of
        AAbs name ae1 -> EAbs name $ skipLambdas f ae1
        _             -> f (free, ae)

  -- Lift lambda into let and bind free variables
  AAbs parms  e -> foldl EApp sc $ map EId freeList
    where
      freeList = Set.toList free
      sc  = ELet [Bind "sc" [] rhs] $ EId "sc"
      rhs = EAbs (freeList ++ parms) $ abstractExp e

-- | Rename all supercombinators and variables
rename :: Program -> Program
rename (Program ds) = Program $ map (uncurry3 Bind) tuples
  where
    tuples = snd (mapAccumL renameSc 0 ds)
    renameSc i (Bind n xs e) = (i2, (n, xs', e'))
      where
        (i1, xs', env) = newNames i xs
        (i2, e')       = renameExp env i1 e

renameExp :: Map Ident Ident -> Int -> Exp -> (Int, Exp)
renameExp env i = \case

  EId  n     -> (i, EId . fromMaybe n $ Map.lookup n env)

  EInt i1    -> (i, EInt i1)

  EApp e1 e2 -> (i2, EApp e1' e2')
    where
      (i1, e1') = renameExp env i e1
      (i2, e2') = renameExp env i1 e2

  EAdd e1 e2 -> (i2, EAdd e1' e2')
    where
      (i1, e1') = renameExp env i e1
      (i2, e2') = renameExp env i1 e2

  ELet bs e  -> (i3, ELet (zipWith3 Bind ns' xs es') e')
    where
      (i1, e')        = renameExp e_env i e
      (ns, xs, es)    = fromBinders bs
      (i2, ns', env') = newNames i1 ns
      e_env           = Map.union env' env
      (i3, es')       = mapAccumL (renameExp e_env) i2 es


  EAbs parms  e  -> (i2, EAbs ns e')
    where
      (i1, ns, env') = newNames i parms
      (i2, e')       = renameExp (Map.union env' env ) i1 e


newNames :: Int -> [Ident] -> (Int, [Ident], Map Ident Ident)
newNames i old_names = (i', new_names, env)
  where
    (i', new_names) = getNames i old_names
    env = Map.fromList $ zip old_names new_names

getNames :: Int -> [Ident] -> (Int, [Ident])
getNames i ns = (i + length ss, zipWith makeName ss [i..])
  where
    ss = map (\(Ident s) -> s) ns

makeName :: String -> Int -> Ident
makeName prefix i = Ident (prefix ++ "_" ++ show i)


-- | Collects supercombinators by lifting appropriate let expressions
collectScs :: Program -> Program
collectScs (Program scs) = Program $ concatMap collectFromRhs scs
  where
    collectFromRhs (Bind name parms rhs) =
      let (rhs_scs, rhs') = collectScsExp rhs
      in  Bind name parms rhs' : rhs_scs


collectScsExp :: Exp -> ([Bind], Exp)
collectScsExp = \case
  EId  n     -> ([], EId n)
  EInt i     -> ([], EInt i)

  EApp e1 e2 -> (scs1 ++ scs2, EApp e1' e2')
    where
      (scs1, e1') = collectScsExp e1
      (scs2, e2') = collectScsExp e2

  EAdd e1 e2 -> (scs1 ++ scs2, EAdd e1' e2')
    where
      (scs1, e1') = collectScsExp e1
      (scs2, e2') = collectScsExp e2

  EAbs x  e  -> (scs, EAbs x e')
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
  ELet binds e  -> (binds_scs ++ rhss_scs ++ e_scs, mkEAbs non_scs' e')
    where
      binds_scs           = [ Bind n (parms ++ parms1) e1
                            | Bind n parms (EAbs parms1 e1) <- scs'
                            ]
      (rhss_scs, binds')  = mapAccumL collectScsRhs [] binds
      (e_scs, e')         = collectScsExp e

      (scs', non_scs')    = partition (\(Bind _ _ rhs) -> isEAbs rhs) binds'

      collectScsRhs acc (Bind n xs rhs) = (acc ++ rhs_scs, Bind n xs rhs')
        where
          (rhs_scs, rhs') = collectScsExp rhs

isEAbs :: Exp -> Bool
isEAbs = \case
  EAbs {} -> True
  _       -> False

mkEAbs :: [Bind] -> Exp -> Exp
mkEAbs [] e = e
mkEAbs bs e = ELet bs e
