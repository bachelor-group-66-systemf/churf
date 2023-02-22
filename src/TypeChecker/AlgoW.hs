{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use traverse_" #-}

module TypeChecker.AlgoW  where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor        (bimap, second)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.List             (foldl', intersect)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Set              (Set)
import qualified Data.Set              as S

import           Grammar.Abs
import           Grammar.Print         (Print, printTree)
import qualified TypeChecker.HMIr      as T

-- | A data type representing type variables
data Poly = Forall [Ident] Type
    deriving Show

newtype Ctx = Ctx { vars  :: Map Ident Poly }

data Env = Env { count :: Int
               , sigs  :: Map Ident Type
               }

type Error = String
type Subst = Map Ident Type

type Infer = StateT Env (ReaderT Ctx (ExceptT Error Identity))

initCtx = Ctx mempty
initEnv = Env 0 mempty

runPretty :: Print a => Infer a -> Either Error String
runPretty = fmap printTree . run

run :: Infer a -> Either Error a
run = runC initEnv initCtx

runC :: Env -> Ctx -> Infer a -> Either Error a
runC e c = runIdentity . runExceptT . flip runReaderT c . flip evalStateT e

typecheck :: Program -> Either Error T.Program
typecheck = run . checkPrg

checkPrg :: Program -> Infer T.Program
checkPrg (Program bs) = do
    traverse (\(Bind n t _ _ _) -> insertSig n t) bs
    bs' <- mapM checkBind bs
    return $ T.Program bs'

checkBind :: Bind -> Infer T.Bind
checkBind (Bind n t _ args e) = do
    (t', e') <- inferExp $ makeLambda e (reverse args)
    s <- unify t t'
    let t'' = apply s t
    return $ T.Bind (t'',n) [] e'
  where
    makeLambda :: Exp -> [Ident] -> Exp
    makeLambda = foldl (flip EAbs)

inferExp :: Exp -> Infer (Type, T.Exp)
inferExp e = do
    (s, t, e') <- w e
    let subbed = apply s t
    return (subbed, replace subbed e')

replace :: Type -> T.Exp -> T.Exp
replace t = \case
    T.EInt t' e -> T.EInt t e
    T.EId t' i -> T.EId t i
    T.EAbs t' name e -> T.EAbs t name e
    T.EApp t' e1 e2 -> T.EApp t e1 e2
    T.EAdd t' e1 e2 -> T.EAdd t e1 e2
    T.ELet t' name e1 e2 -> T.ELet t name e1 e2

w :: Exp -> Infer (Subst, Type, T.Exp)
w = \case
    EAnn e t -> do
        (s1, t', e') <- w e
        applySt s1 $ do
            s2 <- unify (apply s1 t) t'
            return (s2 `compose` s1, t, e')
    EInt n -> return (nullSubst, TMono "Int", T.EInt (TMono "Int") n)
    EId i -> do
        var <- asks vars
        case M.lookup i var of
          Nothing -> throwError $ "Unbound variable: " ++ show i
          Just t  ->  inst t >>= \x -> return (nullSubst, x, T.EId x i)
    EAbs name e -> do
        fr <- fresh
        withBinding name (Forall [] fr) $ do
            (s1, t', e') <- w e
            let newArr = TArr (apply s1 fr) t'
            return (s1, newArr, T.EAbs newArr name e')
    EAdd e0 e1 -> do
        (s1, t0, e0') <- w e0
        applySt s1 $ do
            (s2, t1, e1') <- w e1
            applySt s2 $ do
                s3 <- unify (subst s2 t0) (TMono "Int")
                s4 <- unify (subst s3 t1) (TMono "Int")
                return (s4 `compose` s3 `compose` s2 `compose` s1, TMono "Int", T.EAdd (TMono "Int") e0' e1')
    EApp e0 e1 -> do
        fr <- fresh
        (s1, t0, e0') <- w e0
        applySt s1 $ do
            (s2, t1, e1') <- w e1
            applySt s2 $ do
                s3 <- unify (subst s2 t0) (TArr t1 fr)
                let t = apply s3 fr
                return (s3 `compose` s2 `compose` s1, t, T.EApp t e0' e1')
    ELet name e0 e1 -> do
        (s1, t1, e0') <- w e0
        applySt s1 $ do
            env <- asks vars
            let t' = generalize (apply s1 env) t1
            withBinding name t' $ do
                (s2, t2, e1') <- w e1
                return (s2 `compose` s1, t2, T.ELet t2 name e0' e1' )

-- | Unify two types producing a new substitution (constraint)
unify :: Type -> Type -> Infer Subst
unify t0 t1 = case (t0, t1) of
    (TArr a b, TArr c d) -> do
        s1 <- unify a c
        s2 <- unify (subst s1 b) (subst s1 c)
        return $ s1 `compose` s2
    (TPol a, b) -> occurs a b
    (a, TPol b) -> occurs b a
    (TMono a, TMono b) -> if a == b then return M.empty else throwError "Types do not unify"
    (a, b) -> throwError . unwords $ ["Type:", printTree a, "can't be unified with:", printTree b]

-- | Check if a type is contained in another type.
-- I.E. { a = a -> b } is an unsolvable constraint since there is no substitution such that these are equal
occurs :: Ident -> Type -> Infer Subst
occurs i (TPol a) = return nullSubst
occurs i t = if S.member i (free t)
                then throwError "Occurs check failed"
                else return $ M.singleton i t

-- | Generalize a type over all free variables in the substitution set
generalize :: Map Ident Poly -> Type -> Poly
generalize env t = Forall (S.toList $ free t S.\\ free env) t

-- | Instantiate a polymorphic type. The free type variables are substituted with fresh ones.
inst :: Poly -> Infer Type
inst (Forall xs t) = do
    xs' <- mapM (const fresh) xs
    let s = M.fromList $ zip xs xs'
    return $ apply s t

compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (subst m1) m2 `M.union` m1

-- | A class representing free variables functions
class FreeVars t where
  -- | Get all free variables from t
  free :: t -> Set Ident
  -- | Apply a substitution to t
  apply :: Subst -> t -> t

instance FreeVars Type where
  free :: Type -> Set Ident
  free (TPol a)   = S.singleton a
  free (TMono _)  = mempty
  free (TArr a b) = free a `S.union` free b
  apply :: Subst -> Type -> Type
  apply sub t = do
    case t of
        TMono a -> TMono a
        TPol a -> case M.lookup a sub of
                   Nothing -> TPol a
                   Just t  -> t
        TArr a b -> TArr (apply sub a) (apply sub b)

instance FreeVars Poly where
  free :: Poly -> Set Ident
  free (Forall xs t) = free t S.\\ S.fromList xs
  apply :: Subst -> Poly -> Poly
  apply s (Forall xs t) = Forall xs (apply (foldr M.delete s xs) t)

instance FreeVars (Map Ident Poly) where
  free :: Map Ident Poly -> Set Ident
  free m = foldl' S.union S.empty (map free $ M.elems m)
  apply :: Subst -> Map Ident Poly -> Map Ident Poly
  apply s = M.map (apply s)

applySt :: Subst -> Infer a -> Infer a
applySt s = local (\st -> st { vars = apply s (vars st) })

-- | Represents the empty substition set
nullSubst :: Subst
nullSubst = M.empty

-- | Substitute type variables with their mappings from the substitution set.
subst :: Subst -> Type -> Type
subst m t = do
    case t of
        TPol a   -> fromMaybe t (M.lookup a m)
        TMono a  -> TMono a
        TArr a b -> TArr (subst m a) (subst m b)

-- | Generate a new fresh variable and increment the state counter
fresh :: Infer Type
fresh = do
    n <- gets count
    modify (\st -> st { count = n + 1 })
    return . TPol . Ident $ "t" ++ show n

-- | Run the monadic action with an additional binding
withBinding :: (Monad m, MonadReader Ctx m) => Ident -> Poly -> m a -> m a
withBinding i p = local (\st -> st { vars = M.insert i p (vars st) })

-- | Insert a function signature into the environment
insertSig :: Ident -> Type -> Infer ()
insertSig i t = modify (\st -> st { sigs = M.insert i t (sigs st) })

-- | Lookup a variable in the context
lookupVar :: Ident -> Infer Poly
lookupVar i = do
    m <- asks vars
    case M.lookup i m of
        Just t  -> return t
        Nothing -> throwError $ "Unbound variable: " ++ show i

lett = let (Right (t,e)) = run $ inferExp $ ELet "x" (EAdd (EInt 5) (EInt 5)) (EAdd (EId "x") (EId "x"))
        in t == TMono "Int"

letty = let (Right (t,e)) = run $ inferExp $ ELet "f" (EAbs "x" (EId "x")) (EApp (EId "f") (EInt 3))
         in e
