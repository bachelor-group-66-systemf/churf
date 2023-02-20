{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.AlgoW where

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
import           Grammar.Print         (printTree)
import qualified TypeChecker.HMIr      as T

data Poly = Forall [Ident] Type
    deriving Show

data Ctx = Ctx { vars :: Map Ident Poly
               , sigs :: Map Ident Type }

type Error = String
type Subst = Map Ident Type

type Infer = StateT Int (ReaderT Ctx (ExceptT Error Identity))

initCtx = Ctx mempty mempty

run :: Infer a -> Either Error a
run = runC initCtx 0

runC :: Ctx -> Int -> Infer a -> Either Error a
runC c e = runIdentity . runExceptT . flip runReaderT c . flip evalStateT e

inferExp :: Exp -> Infer Type
inferExp e = snd <$> w nullSubst e

w :: Subst -> Exp -> Infer (Subst, Type)
w s = \case
    EAnn e t -> do
        (s1, t') <- w nullSubst e
        let t'' = apply s1 t
        return (s1, t'')
    EInt n -> return (nullSubst, TMono "Int")
    EId i -> do
        var <- asks vars
        case M.lookup i var of
          Nothing -> throwError $ "Unbound variable: " ++ show i
          Just t  -> (nullSubst,) <$> inst t
    EAbs var e -> do
        fr <- fresh
        withBinding var (Forall [] fr) $ do
            (s1, t') <- w s e
            return (s, TArr (apply s1 fr) t')
    EAdd e0 e1 -> do
        (s1, t1) <- w s e0
        (s2, t2) <- w s1 e1
        return (s2, TMono "Int")
    EApp e0 e1 -> do
        fr <- fresh
        (s1, t0) <- w s e0
        (s2, t1) <- w s1 e1
        s3 <- unify (subst s2 t0) (TArr t1 fr)
        return (s3 `compose` s2 `compose` s1, apply s3 fr)
    ELet name e0 e1 -> do
        (s1, t1) <- w s e0
        env <- asks vars
        let t' = generalize (apply s1 env) t1
        withBinding name t' $ do
            (s2, t2) <- w s1 e1
            return (s1 `compose` s2, t2)

unify :: Type -> Type -> Infer Subst
unify t0 t1 = case (t0, t1) of
    (TArr a b, TArr c d) -> do
        s1 <- unify a c
        s2 <- unify (subst s1 b) (subst s1 c)
        return $ s1 `compose` s2
    (TPol a, b) -> occurs a b
    (a, TPol b) -> occurs b a
    (TMono a, TMono b) -> if a == b then return M.empty else throwError "Types do not unify"

occurs :: Ident -> Type -> Infer Subst
occurs i (TPol a) = return nullSubst
occurs i t = if S.member i (free t)
                then throwError "Occurs check failed"
                else return $ M.singleton i t

generalize :: Map Ident Poly -> Type -> Poly
generalize env t = Forall (S.toList $ free t S.\\ free env) t

inst :: Poly -> Infer Type
inst (Forall xs t) = do
    xs' <- mapM (const fresh) xs
    let s = M.fromList $ zip xs xs'
    return $ apply s t

compose :: Subst -> Subst -> Subst
compose m1 m2 = M.map (subst m1) m2 `M.union` m1

class FreeVars t where
  free :: t -> Set Ident
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

nullSubst :: Subst
nullSubst = M.empty

subst :: Subst -> Type -> Type
subst m t = do
    case t of
        TPol a   -> fromMaybe t (M.lookup a m)
        TMono a  -> TMono a
        TArr a b -> TArr (subst m a) (subst m b)

-- | Generate a new fresh variable and increment the state
fresh :: Infer Type
fresh = do
    n <- get
    put (n + 1)
    return . TPol . Ident $ "t" ++ show n

withBinding :: Ident -> Poly -> Infer (Subst, Type) -> Infer (Subst, Type)
withBinding i t = local (\re -> re { vars = M.insert i t (vars re) })

lookupVar :: Ident -> Infer Poly
lookupVar i = do
    m <- asks vars
    case M.lookup i m of
      Just t  -> return t
      Nothing -> throwError $ "Unbound variable: " ++ show i

{-# WARNING todo "TODO IN CODE" #-}
todo :: a
todo = error "TODO in code"
