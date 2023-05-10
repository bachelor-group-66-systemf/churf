{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}


module LambdaLifter where

import           Auxiliary                 (onM, snoc)
import           Control.Applicative       (Applicative (liftA2))
import           Control.Monad.State       (MonadState (get, put), State,
                                            evalState)
import           Data.Function             (on)
import           Data.List                 (delete, mapAccumL, (\\))
import           Data.Tuple.Extra          (first, second)
import           LambdaLifterIr            (T)
import qualified LambdaLifterIr            as L
import           Prelude                   hiding (exp)
import           TypeChecker.TypeCheckerIr hiding (T)

-- | Lift lambdas and let expression into supercombinators.
-- Three phases:
-- @freeVars@ annotates all the free variables.
-- @abstract@ converts lambdas into let expressions.
-- @collectScs@ moves every non-constant let expression to a top-level function.
--
lambdaLift :: Program -> L.Program
lambdaLift (Program ds) = L.Program (datatypes ++ binds)
  where
    datatypes = [L.DData (toLirData d) | DData d <- ds]

    binds = map L.DBind $ (collectScs . abstract . freeVars) [b | DBind b <- ds]


-- | Annotate free variables
freeVars :: [Bind] -> [ABind]
freeVars binds = [ let ae  = freeVarsExp [] e
                       ae' = ae { frees =  ae.frees \\ xs }
                   in ABind n xs ae'
                 | Bind n xs e <- binds
                 ]

freeVarsExp :: Frees -> T Exp -> Ann (T AExp)
freeVarsExp localVars (ae, t) = case ae of
    EVar  n  | elem (n,t) localVars -> Ann { frees = [(n, t)]
                                           , term = (AVar n, t)
                                           }
             | otherwise            -> Ann { frees = []
                                           , term = (AVar n, t)
                                           }

    EInj n -> Ann { frees = [], term = (AInj n, t) }

    ELit lit -> Ann { frees = [], term = (ALit lit, t) }

    EApp e1 e2 -> Ann { frees = annae1.frees <|| annae2.frees
                      , term = (AApp annae1 annae2, t)
                      }
      where
        (annae1, annae2) = on (,) (freeVarsExp localVars) e1 e2

    EAdd e1 e2 -> Ann { frees = annae1.frees <|| annae2.frees
                      , term = (AAdd annae1 annae2, t)
                      }
      where
        (annae1, annae2) = on (,) (freeVarsExp localVars) e1 e2


    EAbs x e -> Ann { frees = delete (x,t_x) $ annae.frees
                    , term = (AAbs x annae, t) }
      where
        annae = freeVarsExp (localVars <| (x,t_x)) e
        t_x = case t of TFun t _ -> t
                        _        -> error "Impossible"

    -- Sum free variables present in bind and the expression
    -- let f x = x + y in f 5 + z  → frees: y, z
    ELet bind@(Bind n _ _) e  ->
        Ann { frees = delete n annae.frees <|| annbind.frees
            , term = (ALet annbind annae, t)
            }
      where
        annae  = freeVarsExp (localVars <| n) e
        annbind = freeVarsBind localVars bind

    ECase e branches ->
      Ann { frees = foldl (<||) annae.frees (map frees annbranches)
          , term  = (ACase annae annbranches, t)
          }
      where
        annae = freeVarsExp localVars e
        annbranches = map (freeVarsBranch localVars) branches


freeVarsBind :: Frees -> Bind -> Ann ABind
freeVarsBind localVars (Bind name vars e) =
    Ann { frees = annae.frees \\ vars
        , term = ABind name vars annae
        }
  where
    annae = freeVarsExp (localVars <|| vars) e


freeVarsBranch :: Frees -> Branch -> Ann ABranch
freeVarsBranch localVars (Branch pt e) =
  Ann { frees = annae.frees \\ varsInPattern
      , term  = ABranch pt annae
      }
  where
    annae = freeVarsExp (localVars <|| varsInPattern) e
    varsInPattern = go [] pt
      where
        go acc (p, t) = case p of
            PVar n    -> acc <| (n, t)
            PInj _ ps -> foldl go acc ps
            _         -> []


-- AST annotated with free variables

type Frees = [(Ident, Type)]

data Ann a = Ann
  { frees :: Frees
  , term  :: a
  } deriving (Show, Eq)

data ABind = ABind (T Ident) [T Ident] (Ann (T AExp)) deriving (Show, Eq)
data ABranch = ABranch (Pattern, Type) (Ann (T AExp)) deriving (Show, Eq)

data AExp = AVar Ident
            | AInj Ident
            | ALit Lit
            | ALet (Ann ABind) (Ann (T AExp))
            | AApp (Ann (T AExp)) (Ann (T AExp))
            | AAdd (Ann (T AExp)) (Ann (T AExp))
            | AAbs Ident (Ann (T AExp))
            | ACase (Ann (T AExp)) [Ann ABranch]
              deriving (Show, Eq)



data BBind = BBind (T Ident) [T Ident] (T BExp)
           | BBindCxt [T Ident] (T Ident) [T Ident] (T BExp)
    deriving (Eq, Ord, Show)


data BBranch = BBranch (T Pattern) (T BExp)
    deriving (Eq, Ord, Show)

data BExp
    = BVar Ident
    | BVarC [T Ident] Ident
    | BInj Ident
    | BLit Lit
    | BLet BBind (T BExp)
    | BApp (T BExp)(T BExp)
    | BAdd (T BExp)(T BExp)
    | BCase (T BExp) [BBranch]
    deriving (Eq, Ord, Show)


abstract :: [ABind] -> [BBind]
abstract bs = evalState (mapM (abstractAnnBind . Ann []) bs) 0

abstractAnnBind :: Ann ABind -> State Int BBind
abstractAnnBind Ann { term = ABind name vars annae } =
    BBind name (vars' <|| vars) <$> abstractAnnExp annae'
  where
    (annae', vars') = go [] annae
      where
        go acc = \case
            Ann { term = (AAbs x ae, TFun t _) } -> go (snoc (x, t) acc) ae
            ae                                   -> (ae, acc)

abstractAnnExp :: Ann (T AExp) -> State Int (T BExp)
abstractAnnExp Ann {frees, term = (annae, typ) } = case annae of
    AVar n     -> pure (BVar n, typ)
    AInj n     -> pure (BInj  n, typ)
    ALit lit   -> pure (BLit lit, typ)
    AApp annae1 annae2 -> (, typ) <$> onM BApp abstractAnnExp annae1 annae2
    AAdd annae1 annae2 -> (, typ) <$> onM BAdd abstractAnnExp annae1 annae2

    AAbs x annae' -> do
        i <- nextNumber
        rhs <- abstractAnnExp annae''
        let sc_name  = Ident ("sc_" ++ show i)
            sc   | null frees = (BVar sc_name, typ)
                 | otherwise  = (BVarC frees sc_name, typ)
            bind | null frees = BBind (sc_name, typ) vars rhs
                 | otherwise  = BBindCxt frees (sc_name, typ) vars rhs

        pure (BLet bind sc ,typ)

      where
        vars = [(x, t_x)] <|| ys
        t_x = case typ of TFun t _ -> t
                          _        -> error "Impossible"

        (annae'', ys) = go [] annae'
          where
            go acc = \case
                Ann { term = (AAbs x ae, TFun t _) } -> go (snoc (x, t) acc) ae
                ae                                   -> (ae, acc)

    ACase annae' bs -> do
        bs <- mapM go bs
        e  <- abstractAnnExp annae'
        pure (BCase e bs, typ)
      where
        go Ann { term = ABranch p annae } = BBranch p <$> abstractAnnExp annae

    ALet b annae' ->
        (, typ) <$> liftA2 BLet (abstractAnnBind b) (abstractAnnExp annae')


-- | Collects supercombinators by lifting non-constant let expressions
collectScs :: [BBind] -> [L.Bind]
collectScs = concatMap collectFromRhs
  where
    collectFromRhs (BBind name parms rhs) =
        let (rhs_scs, rhs') = collectScsExp rhs
        in  L.Bind name parms rhs' : rhs_scs
    collectFromRhs (BBindCxt cxt name parms rhs) =
        let (rhs_scs, rhs') = collectScsExp rhs
        in  L.BindC cxt name parms rhs' : rhs_scs


collectScsExp :: T BExp -> ([L.Bind], T L.Exp)
collectScsExp (exp, typ) = case exp of
    BVar  x -> ([], (L.EVar x, typ))
    BVarC as x -> ([], (L.EVarC as x, typ))
    BInj k -> ([], (L.EInj k, typ))
    BLit lit -> ([], (L.ELit lit, typ))

    BApp e1 e2 -> (scs1 ++ scs2, (L.EApp  e1' e2', typ))
      where
        (scs1, e1') = collectScsExp e1
        (scs2, e2') = collectScsExp e2

    BAdd e1 e2 -> (scs1 ++ scs2, (L.EAdd e1' e2', typ))
      where
        (scs1, e1') = collectScsExp e1
        (scs2, e2') = collectScsExp e2

    BCase e branches -> (scs ++ scs_e, (L.ECase e' branches', typ))
      where
          (scs, branches') = mapAccumL f [] branches
          (scs_e, e') = collectScsExp e
          f acc b = (acc ++ acc', b')
            where (acc', b') = collectScsBranch b

    -- Collect supercombinators from bind, the rhss, and the expression.
    --
    -- > f = let sc x y = rhs in e
    --
    BLet (BBind name parms rhs) e
        | null parms -> (rhs_scs ++ et_scs, (L.ELet name rhs' et', snd et'))
        | otherwise  -> (bind : rhs_scs ++ et_scs, et')
      where
        bind            = L.Bind name parms rhs'
        (rhs_scs, rhs') = collectScsExp rhs
        (et_scs, et')   = collectScsExp e


    BLet (BBindCxt cxt name parms rhs) e
        | null parms -> (rhs_scs ++ et_scs, (L.ELet name rhs' et', snd et'))
        | otherwise  -> (bind : rhs_scs ++ et_scs, et')
      where
        bind            = L.BindC cxt name parms rhs'
        (rhs_scs, rhs') = collectScsExp rhs
        (et_scs, et')   = collectScsExp e

collectScsBranch (BBranch patt exp) = (scs, L.Branch (first toLirPattern patt) exp')
  where (scs, exp') = collectScsExp exp

nextNumber :: State Int Int
nextNumber = do
    i <- get
    put $ succ i
    pure i


(<|) :: Eq a => [a] -> a -> [a]
xs <| x | elem x xs = xs
        | otherwise = snoc x xs

(<||) :: Eq a => [a] -> [a] -> [a]
xs <|| ys = foldl (<|) xs ys

toLirData (Data t injs) = L.Data t (map toLirInj injs)
toLirInj (Inj n t) = L.Inj n t

toLirPattern :: Pattern -> L.Pattern
toLirPattern = \case
    PVar x    -> L.PVar x
    PLit lit  -> L.PLit lit
    PCatch    -> L.PCatch
    PEnum k   -> L.PEnum k
    PInj k ps -> L.PInj k (map (first toLirPattern) ps)
