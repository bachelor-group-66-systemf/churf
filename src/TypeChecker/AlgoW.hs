{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.AlgoW where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor        (bimap, second)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.List             (intersect)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)

import           Grammar.Abs
import qualified TypeChecker.HMIr      as T

data Poly = Forall [Ident] Type
    deriving Show

a = TPol "a"
b = TPol "b"
int = TMono "int"
arr = TArr

data Ctx = Ctx { vars :: Map Ident Poly
               , sigs :: Map Ident Poly }

data Env = Env { counter       :: Int
               , substitutions :: Map Type Type
               }

type Subst = Map Type Type
type Error = String

newtype Infer a = Infer { runInfer :: StateT Env (ReaderT Ctx (ExceptT Error Identity)) a }
    deriving (Functor, Applicative, Monad, MonadState Env, MonadReader Ctx, MonadError Error)

initCtx :: Ctx
initCtx = Ctx mempty mempty

initEnv :: Env
initEnv = Env 0 mempty

run :: Ctx -> Env -> Infer a -> Either Error a
run c e = runIdentity . runExceptT . flip runReaderT c . flip evalStateT e . runInfer

w :: Exp -> Infer Type
w = \case
    EInt n -> return int
    EId i -> (\(Forall _ t) -> t) <$> (lookupVar i >>= inst)
    EAbs var e -> do
        fr <- fresh
        withBinding var (Forall [] (TPol fr)) $ do
            t' <- w e
            subst (Forall [] $ TArr (TPol fr) t')
    EApp e0 e1 -> do
        t0 <- substCtx (w e0)
        t1 <- w e1
        undefined

substCtx :: Infer Type -> Infer Type
substCtx m = do
    vs <- asks (M.toList . vars)
    ks <- traverse (subst . snd) vs
    let x = map fst vs
    local (\st -> st { vars = M.fromList $ zip x ks }) m

subst :: Poly -> Infer Poly
subst (Forall xs t) = do
    subs <- gets substitutions
    case t of
        TPol a -> case M.lookup (TPol a) subs of
                    Nothing -> return $ Forall xs t
                    Just t' -> return $ Forall (remove a xs) t'
        TMono a -> case M.lookup (TMono a) subs of
                    Nothing -> return $ Forall xs t
                    Just t' -> return $ Forall (remove a xs) t'
        TArr a b -> do
            (Forall xs' a') <- subst (Forall xs a)
            (Forall xs'' b') <- subst (Forall xs b)
            return $ Forall (xs' `intersect` xs'') (TArr a' b')


remove :: Ord a => a -> [a] -> [a]
remove a = foldr (\x acc -> if x == a then acc else x : acc) []

inst :: Poly -> Infer Poly
inst (Forall xs t) = do
    xs' <- mapM (const fresh) xs
    let sub = zip xs xs'
    let subst' t = case t of
            TMono a -> return $ TMono a
            TPol a -> case lookup a sub of
                       Nothing -> return $ TPol a
                       Just t  -> return $ TPol t
            TArr a b -> TArr <$> subst' a <*> subst' b
    Forall [] <$> subst' t

-- | Generate a new fresh variable and increment the state
fresh :: Infer Ident
fresh = do
    n <- gets counter
    modify (\st -> st { counter = n + 1 })
    return . Ident $ "t" ++ show n

insertSub :: Type -> Type -> Infer ()
insertSub t1 t2 = modify (\st -> st { substitutions = M.insert t1 t2 (substitutions st) })

withBinding :: Ident -> Poly -> Infer Poly -> Infer Type
withBinding i t m = (\(Forall _ t) -> t) <$> local (\re -> re { vars = M.insert i t (vars re) }) m

lookupVar :: Ident -> Infer Poly
lookupVar i = do
    m <- asks vars
    case M.lookup i m of
      Just t  -> return t
      Nothing -> throwError $ "Unbound variable: " ++ show i

{-# WARNING todo "TODO IN CODE" #-}
todo :: a
todo = error "TODO in code"
