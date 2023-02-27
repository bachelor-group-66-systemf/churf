--{-# LANGUAGE LambdaCase        #-}
--{-# LANGUAGE OverloadedStrings #-}


module LambdaLifter.LambdaLifter where

--import           Auxiliary                 (snoc)
--import           Control.Applicative       (Applicative (liftA2))
--import           Control.Monad.State       (MonadState (get, put), State,
--                                            evalState)
--import           Data.Set                  (Set)
--import qualified Data.Set                  as Set
--import           Prelude                   hiding (exp)
--import           Renamer.Renamer
--import           TypeChecker.TypeCheckerIr


---- | Lift lambdas and let expression into supercombinators.
---- Three phases:
---- @freeVars@ annotatss all the free variables.
---- @abstract@ converts lambdas into let expressions.
---- @collectScs@ moves every non-constant let expression to a top-level function.
--lambdaLift :: Program -> Program
--lambdaLift = collectScs . abstract . freeVars


---- | Annotate free variables
--freeVars :: Program -> AnnProgram
--freeVars (Program ds) = [ (n, xs, freeVarsExp (Set.fromList xs) e)
--                        | Bind n xs e <- ds
--                        ]

--freeVarsExp :: Set Id -> Exp -> AnnExp
--freeVarsExp localVars = \case
--    EId  n  | Set.member n localVars -> (Set.singleton n, AId n)
--            | otherwise              -> (mempty, AId n)

--    ELit _ (LInt i) -> (mempty, AInt i)

--    EApp t e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AApp t e1' e2')
--      where
--        e1' = freeVarsExp localVars e1
--        e2' = freeVarsExp localVars e2

--    EAdd t e1 e2 -> (Set.union (freeVarsOf e1') (freeVarsOf e2'), AAdd t e1' e2')
--      where
--        e1' = freeVarsExp localVars e1
--        e2' = freeVarsExp localVars e2

--    EAbs t par e -> (Set.delete par $ freeVarsOf e', AAbs t par e')
--      where
--        e' = freeVarsExp (Set.insert par localVars) e

--    -- Sum free variables present in bind and the expression
--    ELet (Bind name parms rhs) e  -> (Set.union binders_frees e_free, ALet new_bind e')
--      where
--        binders_frees = Set.delete name $ freeVarsOf rhs'
--        e_free        = Set.delete name $ freeVarsOf e'

--        rhs'     = freeVarsExp e_localVars rhs
--        new_bind = ABind name parms rhs'

--        e'          = freeVarsExp e_localVars e
--        e_localVars = Set.insert name localVars


--freeVarsOf :: AnnExp -> Set Id
--freeVarsOf = fst

---- AST annotated with free variables
--type AnnProgram = [(Id, [Id], AnnExp)]

--type AnnExp = (Set Id, AnnExp')

--data ABind = ABind Id [Id] AnnExp deriving Show

--data AnnExp' = AId  Id
--             | AInt Integer
--             | ALet ABind AnnExp
--             | AApp Type    AnnExp  AnnExp
--             | AAdd Type    AnnExp  AnnExp
--             | AAbs Type    Id      AnnExp
--             deriving Show
---- | Lift lambdas to let expression of the form @let sc = \v₁ x₁ -> e₁@.
---- Free variables are @v₁ v₂ .. vₙ@ are bound.
--abstract :: AnnProgram -> Program
--abstract prog = Program $ evalState (mapM go prog) 0
--  where
--    go :: (Id, [Id], AnnExp) -> State Int Bind
--    go (name, parms, rhs) = Bind name (parms ++ parms1) <$> abstractExp rhs'
--      where
--        (rhs', parms1) = flattenLambdasAnn rhs


---- | Flatten nested lambdas and collect the parameters
---- @\x.\y.\z. ae → (ae, [x,y,z])@
--flattenLambdasAnn :: AnnExp -> (AnnExp, [Id])
--flattenLambdasAnn ae = go (ae, [])
--  where
--    go :: (AnnExp, [Id]) -> (AnnExp, [Id])
--    go ((free, e), acc) =
--        case e of
--            AAbs _ par (free1, e1) ->
--                go ((Set.delete par free1, e1), snoc par acc)
--            _ -> ((free, e), acc)

--abstractExp :: AnnExp -> State Int Exp
--abstractExp (free, exp) = case exp of
--    AId  n       -> pure $ EId  n
--    AInt i       -> pure $ ELit (TMono "Int") (LInt i)
--    AApp t e1 e2 -> liftA2 (EApp t) (abstractExp e1) (abstractExp e2)
--    AAdd t e1 e2 -> liftA2 (EAdd t) (abstractExp e1) (abstractExp e2)
--    ALet b e     -> liftA2 ELet (go b) (abstractExp e)
--      where
--        go (ABind name parms rhs) = do
--            (rhs', parms1)  <- flattenLambdas <$> skipLambdas abstractExp rhs
--            pure $ Bind name (parms ++ parms1) rhs'

--        skipLambdas :: (AnnExp -> State Int Exp) -> AnnExp -> State Int Exp
--        skipLambdas f (free, ae) = case ae of
--            AAbs t par ae1 -> EAbs t par <$> skipLambdas f ae1
--            _              -> f (free, ae)

--    -- Lift lambda into let and bind free variables
--    AAbs t parm e -> do
--        i <- nextNumber
--        rhs <- abstractExp e

--        let sc_name = Ident ("sc_" ++ show i)
--            sc      = ELet (Bind (sc_name, t) parms rhs) $ EId (sc_name, t)

--        pure $ foldl (EApp $ TMono "Int") sc $ map EId freeList
--      where
--        freeList = Set.toList free
--        parms    = snoc parm freeList


--nextNumber :: State Int Int
--nextNumber = do
--    i <- get
--    put $ succ i
--    pure i

---- | Collects supercombinators by lifting non-constant let expressions
--collectScs :: Program -> Program
--collectScs (Program scs) = Program $ concatMap collectFromRhs scs
--  where
--    collectFromRhs (Bind name parms rhs) =
--        let (rhs_scs, rhs') = collectScsExp rhs
--        in  Bind name parms rhs' : rhs_scs


--collectScsExp :: Exp -> ([Bind], Exp)
--collectScsExp = \case
--    EId  n -> ([], EId n)
--    ELit _ (LInt i) -> ([], ELit (TMono "Int") (LInt i))

--    EApp t e1 e2 -> (scs1 ++ scs2, EApp t e1' e2')
--      where
--        (scs1, e1') = collectScsExp e1
--        (scs2, e2') = collectScsExp e2

--    EAdd t e1 e2 -> (scs1 ++ scs2, EAdd t e1' e2')
--      where
--        (scs1, e1') = collectScsExp e1
--        (scs2, e2') = collectScsExp e2

--    EAbs t par e -> (scs, EAbs t par e')
--      where
--        (scs, e') = collectScsExp e

--    -- Collect supercombinators from bind, the rhss, and the expression.
--    --
--    -- > f = let sc x y = rhs in e
--    --
--    ELet (Bind name parms rhs) e -> if null parms
--                                    then (       rhs_scs ++ e_scs, ELet bind e')
--                                    else (bind : rhs_scs ++ e_scs, e')
--      where
--        bind            = Bind name parms rhs'
--        (rhs_scs, rhs') = collectScsExp rhs
--        (e_scs, e')     = collectScsExp e


---- @\x.\y.\z. e → (e, [x,y,z])@
--flattenLambdas :: Exp -> (Exp, [Id])
--flattenLambdas = go . (, [])
--  where
--    go (e, acc) = case e of
--                      EAbs _ par e1 -> go (e1, snoc par acc)
--                      _             -> (e, acc)

