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
import           Prelude                   hiding (exp)
import           TypeChecker.TypeCheckerIr


-- | Lift lambdas and let expression into supercombinators.
-- Three phases:
-- @freeVars@ annotates all the free variables.
-- @abstract@ converts lambdas into let expressions.
-- @collectScs@ moves every non-constant let expression to a top-level function.
--
lambdaLift :: Program -> Program
lambdaLift (Program ds) = Program (datatypes ++ binds)
  where
    datatypes = flip filter ds $ \case DData _ -> True
                                       _       -> False
    binds = map DBind $ (collectScs . abstract . freeVars) [b | DBind b <- ds]

-- lambdaLift (Program defs) = trace (printTree abst) $ Program $ datatypes ++ ll binds
--   where
--     abst = abstract frees
--     frees = freeVars [b | DBind b@(Bind (Ident s, _) _ _) <- binds, s == "f"]
--
--     ll = map DBind . collectScs . abstract . freeVars . map (\(DBind b) -> b)
--     (binds, datatypes) = partition isBind defs
--     isBind = \case
--       DBind _ -> True
--       _       -> False

-- | Annotate free variables
freeVars :: [Bind] -> [ABind]
freeVars binds = [ let ae  = freeVarsExp [] e
                       ae' = ae { frees =  ae.frees \\ xs }
                   in ABind n xs ae'
                 | Bind n xs e <- binds
                 ]

freeVarsExp :: Frees -> ExpT -> Ann AExpT
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
    annae = freeVarsExp localVars e
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

data ABind = ABind Id [Id] (Ann AExpT) deriving (Show, Eq)
data ABranch = ABranch (Pattern, Type) (Ann AExpT) deriving (Show, Eq)

type AExpT = (AExp, Type)

data AExp = AVar Ident
            | AInj Ident
            | ALit Lit
            | ALet (Ann ABind)  (Ann AExpT)
            | AApp (Ann AExpT)  (Ann AExpT)
            | AAdd (Ann AExpT)  (Ann AExpT)
            | AAbs Ident        (Ann AExpT)
            | ACase (Ann AExpT) [Ann ABranch]
              deriving (Show, Eq)

abstract :: [ABind] -> [Bind]
abstract bs = evalState (mapM (abstractAnnBind . Ann []) bs) 0

abstractAnnBind :: Ann ABind -> State Int Bind
abstractAnnBind Ann { term = ABind name vars annae } =
    Bind name (vars' <|| vars) <$> abstractAnnExp annae'
  where
    (annae', vars') = go [] annae
      where
        go acc = \case
            Ann { term = (AAbs x ae, TFun t _) } -> go (snoc (x, t) acc) ae
            ae                                   -> (ae, acc)

abstractAnnExp :: Ann AExpT -> State Int ExpT
abstractAnnExp Ann {frees, term = (annae, typ) } = case annae of
    AVar n     -> pure (EVar n, typ)
    AInj n     -> pure (EInj  n, typ)
    ALit lit   -> pure (ELit lit, typ)
    AApp annae1 annae2 -> (, typ) <$> onM EApp abstractAnnExp annae1 annae2
    AAdd annae1 annae2 -> (, typ) <$> onM EAdd abstractAnnExp annae1 annae2

    -- \x. \y. x + y + z ⇒ let sc x y z = x + y + z in sc
    AAbs x annae' -> do
        i <- nextNumber
        rhs <- abstractAnnExp annae''
        let sc_name = Ident ("sc_" ++ show i)
            sc      = (ELet (Bind (sc_name, typ) vars rhs) (EVar sc_name, typ), typ)
        pure $ foldl applyFree sc frees

      where
        vars = frees <| (x, t_x) <|| ys
        t_x = case typ of TFun t _ -> t
                          _        -> error "Impossible"

        (annae'', ys) = go [] annae'
          where
            go acc = \case
                Ann { term = (AAbs x ae, TFun t _) } -> go (snoc (x, t) acc) ae
                ae                                   -> (ae, acc)


        applyFree :: (Exp' Type, Type) -> (Ident, Type) -> (Exp' Type, Type)
        applyFree (e, t_e) (x, t_x) = (EApp (e, t_e) (EVar x, t_x), t_e')
          where
            t_e' = case t_e of TFun _ t -> t
                               _        -> error "Impossible"

    ACase annae' bs -> do
        bs <- mapM go bs
        e  <- abstractAnnExp annae'
        pure (ECase e bs, typ)
      where
        go Ann { term = ABranch p annae } = Branch p <$> abstractAnnExp annae

    ALet b annae' ->
        (, typ) <$> liftA2 ELet (abstractAnnBind b) (abstractAnnExp annae')


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
    ELet (Bind name parms rhs) e
        | null parms -> (rhs_scs ++ et_scs, (ELet bind et', snd et'))
        | otherwise  -> (bind : rhs_scs ++ et_scs, et')
      where
        bind            = Bind name parms rhs'
        (rhs_scs, rhs') = collectScsExp rhs
        (et_scs, et')   = collectScsExp e

collectScsBranch (Branch patt exp) = (scs, Branch patt exp')
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

