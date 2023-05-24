{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Monomorphizer.Monomorphizer2 (monomorphize) where

import           Auxiliary                     (caseMaybe, for, maybeToRightM,
                                                onM, onMM, snoc)
import           Control.Applicative           (Applicative (liftA2), (<|>))
import           Control.Monad.Except          (ExceptT,
                                                MonadError (throwError),
                                                runExceptT, when)
import           Control.Monad.State           (MonadState (get), State,
                                                execState, gets, modify,
                                                runState)
import           Data.Bifoldable               (Bifoldable (bifoldMap))
import           Data.Bifunctor                (Bifunctor (bimap, first, second))
import           Data.Foldable                 (foldr')
import           Data.Foldable.Extra           (find, notNull)
import           Data.Function                 (on)
import           Data.List                     (intercalate, intersect,
                                                intersperse, nub)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes, fromJust, fromMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Debug.Trace                   (trace)
import           Grammar.ErrM                  (Err)
import           Grammar.Print                 (Print (..), concatD, doc,
                                                printTree)
import           LambdaLifterIr
import qualified Monomorphizer.MonomorphizerIr as M

--  PROGRAM
--
--  .+ : Int -> Int -> Int
--  .+ x y = 0
--
--  const : a -> b -> a
--  const x y = x
--
--  applyId : (forall a. a -> a) -> b -> b
--  applyId f x = const (f x) (f 'B')
--
--  id : a -> a
--  id x = x
--
--  main = applyId id 5 + const 10 (applyId id 'A')
--
--  LAMBDA LIFTER
--
--  ($plus$ : Int -> Int -> Int) ($5x : Int) ($6y : Int) = (0 : Int);
--  (const : forall $0a . forall $1b . $0a -> $1b -> $0a) ($7x : $0a) ($8y : $1b) = ($7x : $0a);
--  (id : forall $4a . $4a -> $4a) ($11x : $4a) = ($11x : $4a);
--  (applyId : forall $2b . (forall $3a . $3a -> $3a) -> $2b -> $2b) ($9f : forall $3a . $3a -> $3a) ($10x : $2b) = (((const : $2b -> Char -> $2b) ((($9f : $2b -> $2b) ($10x : $2b)) : $2b) : Char -> $2b) ((($9f : Char -> Char) ('B' : Char)) : Char) : $2b);
--  (main : Int) = ((($plus$ : Int -> Int -> Int) ((((applyId : ($3a -> $3a) -> Int -> Int) (id : $3a -> $3a) : Int -> Int) (5 : Int)) : Int) : Int -> Int) ((((const : Int -> Char -> Int) (10 : Int) : Char -> Int) ((((applyId : ($3a -> $3a) -> Char -> Char) (id : $3a -> $3a) : Char -> Char) ('A' : Char)) : Char)) : Int) : Int)
--
--  AFTER
--
--  .+ : Int -> Int -> Int
--  .+ x y = 0
--
--  const$CharCharChar : Char -> Char -> Char
--  const$CharCharChar x y = x
--
--  const$IntCharInt : Int -> Char -> Int
--  const$IntCharInt x y = x
--
--  const : a -> Char -> a
--  const x y = x
--
--  applyId$IntIntCharCharIntInt : (Int -> Int) -> (Char -> Char) -> Int -> Int
--  applyId$IntIntCharCharIntInt f$IntInt f$CharChar x = const (f$IntInt x) (f$CharChar 'B')
--
--  applyId$CharCharIntInt : (Char -> Char) -> Char -> Char
--  applyId$CharCharIntInt f$IntInt x = const (f$CharChar x) (f$CharChar 'B')
--
--  id$CharChar : Char -> Char
--  id$CharChar x = x
--
--  id$IntInt : Int -> Int
--  id$IntInt x = x
--
--  main = applyId$IntIntCharCharIntInt id$IntInt id$CharChar 5 + const$CharCharChar 10 (applyId$CharCharIntInt id$CharChar 'A')

monomorphize :: Program ->  Err M.Program
monomorphize p@(Program ds) = monomorphize' p <$> main where
    main | [b] <- xs = pure b
         | otherwise = throwError "No main!"
    xs = [ b | DBind b@(Bind ("main", _) _ _) <- ds]

monomorphize' :: Program -> Bind -> M.Program
monomorphize' (Program ds) main = M.Program $ map M.DBind $ nub copies
  where
    all_constraints = map constraint constraints
    individually_solved = map (\cxt -> cxt{constraint=solveConstraint cxt.constraint}) constraints
    unsolved = [ a | TVar a <- concatMap (Map.elems . constraint) individually_solved]
    unsolved_solved = Map.filterWithKey (const . (`elem` unsolved)) $ solveConstraints all_constraints
    copies = concatMap (uncurry $ generateMonomophic unsolved_solved) needCopies

    log = unlines
        [ "\nCONSTRAINTS: \n" ++ printTree constraints
        , "\nCONSTRAINT SOLVED: \n" ++ printTree individually_solved
        , "\nSOLVED: \n" ++ printTree unsolved_solved
        , "\nNEED COPIES: \n" ++ printTree needCopies
        ]

    initCxt = Cxt
        { binds       = foldr (\b@(Bind (x, _) _ _) -> Map.insert x b) mempty bs
        , vars        = mempty
        , constraints = mempty
        , needCopies  = Set.singleton (main, typeofB main)
        }

    Cxt{constraints, needCopies} = flip execState initCxt
                                 $ runExceptT
                                 $ runMz
                                 $ mapM mzBind bs
    bs = [ b | DBind b <- ds ]

generateMonomophic :: Map TVar (Set Type) -> Bind -> Type -> [M.Bind]
generateMonomophic solutions b t
    | [] <- xs  = [gen solutions $ newType t b]
    | otherwise = map (gen solutions) bs where
        -- bind type alternatives
        bs = for cp
           $ \xs -> up (Map.fromList xs)
           $ newType (mkt xs) b

        mkt = dupHigherRank solutions
            . foldr (uncurry substitute) t

        -- cartesian product of unsolved type variables
        cp = map (zip xs)
           $ sequence
           $ for xs
           $ \x -> Set.toList
           $ fromMaybe (error "uh oh")
           $ Map.lookup x solutions

        -- unsolved type variables
        xs = unsolved t

        newType t (Bind (n, _) xs e) = Bind (n, t) xs e

-- (∀α. α → α) → Int → Int where α ~ Int, Char
-- (Int → Int) → (Char → Char) → Int → Int
dupHigherRank :: Map TVar (Set Type) -> Type -> Type
dupHigherRank solutions typ = go typ where
    go (TFun (TAll a t1) t2)
        | Just ts <- Map.lookup a solutions =
            let (t1', t2') = on (,) go t1 t2 in
            foldr (\t -> TFun (substitute a t t1')) t2' ts
        | otherwise = error "Impossible!"
    go t = case t of
        TVar _     -> t
        TLit _     -> t
        TFun t1 t2 -> on TFun go t1 t2

-- solutions is used for higher rank function calls
gen :: Map TVar (Set Type) -> Bind -> M.Bind
gen solutions b@(Bind (n,t) xs e) = b' where
    b' = M.Bind (genTX (n,t)) xs' (genTE e)
    xs' = nub $ concatMap genVars xs

    genTX :: T Ident -> M.T Ident
    genTX (x, t) = let t' = genT t in (newName x t', t')

    -- TODO maybe needs fix for rank > 2
    genVars (x, TAll a t)
        | Just ts <- Map.lookup a solutions =
            for (Set.toList ts) $ \t' -> genTX (x, substitute a t' t)
        | otherwise = error "Impossible!"
    genVars x = [genTX x]

    genT t = case t of
        TFun (TAll _ _) _ -> genT $ dupHigherRank solutions t
        TFun t1 t2        -> on M.TFun genT t1 t2
        TLit lit          -> M.TLit lit
        TData _ _         -> undefined
        _                 -> error $ "no this " ++ printTree t ++ "\n BIND \n " ++ printTree b

    -- TODO maybe needs fix for rank > 2
    genTE (EApp e1 (e2, TAll a t2), _)
        | Just ts <- Map.lookup a solutions =
            let e1'@(_, typ) = genTE e1
                es  = for (Set.toList ts) $ \t -> genTE (e2, substitute a t t2)
                go e1@(_, M.TFun _ t) e2 = (M.EApp e1 e2, t)
            in  foldl go e1' es
        | otherwise = error "Impossible"

    genTE (e, t) = let t' = genT t in (, t') $ case e of
        EVar x     -> M.EVar $ newName x t'
        EVarC xs x -> M.EVarC (map (second genT) xs) x
        EInj k     -> M.EVar $ newName k t'
        ELit lit   -> M.ELit lit
        -- TODO fix monomorphizerIR remove bind
        -- ELet x e1 e2 -> on (M.ELet x) genTE e1 e2
        EApp e1 e2 -> on M.EApp genTE e1 e2
        ECase e bs -> undefined

newName :: Ident -> M.Type -> Ident
newName "main" = const "main"
newName (Ident s) = Ident
                  . (snoc '$' s ++)
                  . intercalate "_"
                  . go
  where
    go (M.TLit (Ident s)) = [s]
    go (M.TFun t1 t2)     = on (++) go t1 t2

substitute :: TVar  -- α
           -> Type  -- A
           -> Type  -- B
           -> Type  -- [A/α]B
substitute alpha a b = case b of
    TLit _                      -> b
    TVar beta | beta == alpha   -> a
              | otherwise       -> b
    TFun t1 t2                  -> on TFun subs t1 t2
    TAll beta c | beta == alpha -> subs c
                | otherwise     -> TAll beta (subs c)
    TData name typs             -> TData name $ map subs typs
  where
    subs = substitute alpha a

unsolved :: Type -> [TVar]
unsolved = go . skipForalls where
    go = \case
        TFun t1 t2   -> on (<>) go t1 t2
        TData _ typs -> foldr ((<>) . go) [] typs
        TVar a       -> [a]
        TLit _       -> []
        TAll a t     -> filter (/=a) (go t)

skipForalls (TAll _ t) = skipForalls t
skipForalls t          = t

mzBind :: Bind -> Mz ()
mzBind b@(Bind (n,_) xs e) = do
    pushVars
    mapM_ insertVar xs
    flip mz e =<< get
    popVars
  where
    mz :: Cxt -> T Exp -> Mz ()
    mz Cxt{vars,binds} (EVar x, typ)
        | Just t <- lookupVar x vars =
            insertConstraint b x (t :~ typ)
        | Just b' <- Map.lookup x binds = do
            let c = typeofB b' :~ typ
            insertConstraint b x c
            insertNeedCopy b' c

        | otherwise = pure ()

    mz cxt (exp, typ) = case exp of
        EVarC c x    -> undefined
        EInj k       -> undefined
        ELit lit     -> pure ()
        ELet x e1 e2 -> undefined
        EApp e1 e2   -> on (>>) (mz cxt) e1 e2
        ECase e bs   -> undefined

insertNeedCopy :: Bind -> Constraint Type -> Mz ()
insertNeedCopy b c@(_ :~ t) =
    modify $ \cxt -> cxt{needCopies=Set.insert (b',t) cxt.needCopies}
  where
    b' = up (solveConstraint c) b

up :: Map TVar Type -> Bind -> Bind
up solutions b@(Bind x xs e) = Bind x xs' (goTE e) where
    xs' = map (second goT) xs
    goTE :: T Exp -> T Exp
    goTE = bimap goE goT

    goT t = case t of
        TFun t1 t2 -> on TFun goT t1 t2
        TData t ts -> TData t $ map goT ts
        TVar a     -> fromMaybe (TVar a) $ Map.lookup a solutions
        TLit _     -> t
        TAll a t   -> TAll a $ goT t

    goE e = case e of
        EVar _       -> e
        EVarC c x    -> undefined
        EInj _       -> e
        ELit _       -> e
        ELet x e1 e2 -> undefined
        EApp e1 e2   -> on EApp goTE e1 e2
        ECase e bs   -> undefined

insertConstraint :: Bind -> Ident -> Constraint Type -> Mz ()
insertConstraint b x c@(t1 :~ t2)
    | t1 == t2  = pure ()
    | otherwise = modify $ \cxt ->
        cxt{constraints= snoc new cxt.constraints }
  where
    new = ConstraintCxt {bind=b, function=x, constraint=c}

solved :: Type -> Bool
solved = \case
  TAll{}       -> False
  TFun t1 t2   -> on (&&) solved t1 t2
  TData _ typs -> all solved typs
  TVar _       -> False
  TLit _       -> True

lookupVar :: Ident -> [Map Ident Type] -> Maybe Type
lookupVar x (y:ys) = Map.lookup x y <|> lookupVar x ys
lookupVar _ []     = Nothing

typeofB :: Bind -> Type
typeofB (Bind (_, t) _ _) = t

insertVar :: T Ident -> Mz ()
insertVar (x, t) = modify $ \cxt -> cxt{vars = go cxt.vars}
  where
    go []     = error "bad"
    go (y:ys) = Map.insert x t y : ys

pushVars :: Mz ()
pushVars = modify $ \cxt -> cxt{vars = mempty : cxt.vars}

popVars :: Mz ()
popVars = modify $ \cxt -> cxt{vars = go cxt.vars}
  where
    go []     = []
    go (_:bs) = bs

data Cxt = Cxt
    { binds       :: Map Ident Bind
    , vars        :: [Map Ident Type]
    , constraints :: [ConstraintCxt (Constraint Type)]
    , needCopies  :: Set (Bind, Type)
    } deriving Show

data ConstraintCxt a = ConstraintCxt
  { bind       :: Bind
  , function   :: Ident
  , constraint :: a
  } deriving (Show, Eq, Ord)

data Constraint a = a :~ Type deriving (Show, Eq, Ord)

newtype Mz a = Mz { runMz :: ExceptT String (State Cxt) a }
    deriving (Functor, Applicative, Monad, MonadState Cxt, MonadError String)


instance Print a => Print [ConstraintCxt a] where
    prt i = doc . showString . intercalate "\n" . map printTree

instance Print a => Print (ConstraintCxt a) where
    prt i ConstraintCxt{..} = doc $ showString $ toStrBind bind ++ "\n    " ++ printTree function ++ "\n        " ++ printTree constraint

instance Print a => Print (Constraint a) where
    prt i (t1 :~ t2) = doc $ showString $ printTree t1 ++ " :~ " ++ printTree t2

instance Print (Map TVar (Set Type)) where
    prt i = doc . showString . intercalate "\n" . map ((\(x, xs) -> x ++ " :~ " ++ intercalate ", " xs) . bimap printTree (map printTree . Set.toList)) . Map.toList

instance Print (Map TVar Type) where
    prt i = doc . showString . intercalate "\n        " . map ((\(x, y) -> x ++ " :~ " ++ y) . bimap printTree printTree) . Map.toList

instance Print (Set (Bind, Type)) where
    prt i = doc . showString . intercalate "\n" . map (\(b, t) -> toStrBind b ++ " : " ++ printTree t) . Set.toList

toStrBind (Bind (Ident s,_) _ _) = s

solveConstraints :: [Constraint Type] -> Map TVar (Set Type)
solveConstraints cs =
  let adjs = adjacencyDict $ foldr (Set.union . decompose) mempty cs
  in  foldr (\k -> Map.insert k (dfs adjs k)) mempty $ Map.keys adjs

solveConstraint :: Constraint Type -> Map TVar Type
solveConstraint c = go where
    go = foldr (\k -> Map.insert k (dfs' adjs k)) mempty $ Map.keys adjs
    adjs = adjacencyDict $ decompose c

data DfsEnv = DfsEnv
    { visited :: Set TVar
    , leafs   :: Set Type
    , next    :: [Type]
    }

dfs' :: Map TVar [Type] -> TVar -> Type
dfs' ajds = go . dfs ajds where
    go set
        | [t] <- Set.toList set = t
        | otherwise             = error "bad"


dfs :: Map TVar [Type] -> TVar -> Set Type
dfs adjs start = go DfsEnv {visited=Set.singleton start
                           ,leafs=mempty
                           ,next= fromMaybe [] $ Map.lookup start adjs
                           }
  where
    go :: DfsEnv -> Set Type
    go env@DfsEnv{..}
        | [] <- next = leafs
        | n@TLit{}:ns <- next = go env{leafs=Set.insert n leafs, next=ns}
        | TVar a:ns   <- next
        , Just ns' <- Map.lookup a adjs = go env{visited=Set.insert a visited
                                                ,next=ns' ++ ns
                                                }
        | TVar a:ns   <- next
        , Nothing <- Map.lookup a adjs = go env{visited=Set.insert a visited
                                               ,leafs=Set.insert (TVar a) leafs
                                               ,next=ns
                                               }

adjacencyDict :: Set (Constraint TVar) -> Map TVar [Type]
adjacencyDict = Set.foldr go mempty
  where
    go (a :~ t) adjs = Map.insert a (t:ts) adjs
      where
        ts = fromMaybe [] $ Map.lookup a adjs

decompose :: Constraint Type -> Set (Constraint TVar)
decompose (TFun a1 a2 :~ TFun b1 b2) = on (<>) decompose (a1 :~ b1) (a2 :~ b2)
decompose (TAll _ a :~ b)            = decompose (a :~ b)
decompose (a :~ TAll _ b)            = decompose (a :~ b)
decompose (TVar a :~ b)
    | TVar a == b                    = mempty
    | otherwise                      = Set.singleton (a :~ b)
