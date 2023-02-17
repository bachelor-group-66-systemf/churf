{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeChecker.Unification where

import Control.Arrow ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Unification hiding (applyBindings, (=:=))
import Control.Unification qualified as U
import Control.Unification.IntVar
import Data.Foldable (fold)
import Data.Functor.Fixedpoint
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Debug.Trace (trace)
import GHC.Generics (Generic1)
import Renamer.Renamer
import Renamer.RenamerIr (Const (..), Ident (..), RBind (..), RExp (..), RProgram (..))
import Renamer.RenamerIr qualified as R

type Ctx = Map Ident UPolytype

type TypeError = String

data TypeT a = TPolyT Ident | TMonoT Ident | TArrowT a a
    deriving (Functor, Foldable, Traversable, Generic1, Unifiable)

instance Show a => Show (TypeT a) where
    show (TPolyT (Ident i)) = i
    show (TMonoT (Ident i)) = i
    show (TArrowT a b) = show a ++ " -> " ++ show b

type Infer = StateT (Map Ident UPolytype) (ReaderT Ctx (ExceptT TypeError (IntBindingT TypeT Identity)))

type Type = Fix TypeT

type UType = UTerm TypeT IntVar

data Poly t = Forall [Ident] t
    deriving (Eq, Show, Functor)

type Polytype = Poly Type

type UPolytype = Poly UType

pattern TPoly :: Ident -> Type
pattern TPoly v = Fix (TPolyT v)

pattern TMono :: Ident -> Type
pattern TMono v = Fix (TMonoT v)

pattern TArrow :: Type -> Type -> Type
pattern TArrow t1 t2 = Fix (TArrowT t1 t2)

pattern UTMono :: Ident -> UType
pattern UTMono v = UTerm (TMonoT v)

pattern UTArrow :: UType -> UType -> UType
pattern UTArrow t1 t2 = UTerm (TArrowT t1 t2)

pattern UTPoly :: Ident -> UType
pattern UTPoly v = UTerm (TPolyT v)

data TType = TTPoly Ident | TTMono Ident | TTArrow TType TType
    deriving (Show)

newtype Program = Program [Bind]
    deriving (Show)

data Bind = Bind Ident Exp Polytype
    deriving (Show)

data Exp
    = EAnn Exp Polytype
    | EBound Ident Polytype
    | EFree Ident Polytype
    | EConst Const Polytype
    | EApp Exp Exp Polytype
    | EAdd Exp Exp Polytype
    | EAbs Ident Exp Polytype
    deriving (Show)

data TExp
    = TAnn TExp UType
    | TFree Ident UType
    | TBound Ident UType
    | TConst Const UType
    | TApp TExp TExp UType
    | TAdd TExp TExp UType
    | TAbs Ident TExp UType
    deriving (Show)

----------------------------------------------------------
typecheck :: RProgram -> Either TypeError Program
typecheck = run . inferProgram

inferProgram :: RProgram -> Infer Program
inferProgram (RProgram binds) = do
    binds' <- mapM inferBind binds
    return $ Program binds'

inferBind :: RBind -> Infer Bind
inferBind (RBind i e) = do
    (t, e') <- infer e
    e'' <- convert fromUType e'
    t' <- fromUType t
    insertSigs i (Forall [] t)
    return $ Bind i e'' t'

fromUType :: UType -> Infer Polytype
fromUType = applyBindings >>> (>>= (generalize >>> fmap fromUPolytype))

convert :: (UType -> Infer Polytype) -> TExp -> Infer Exp
convert f = \case
    (TAnn e t) -> do
        e' <- convert f e
        EAnn e' <$> f t
    (TFree i t) -> do
        t' <- f t
        return $ EFree i t'
    (TBound i t) -> do
        t' <- f t
        return $ EBound i t'
    (TConst c t) -> do
        t' <- f t
        return $ EConst c t'
    (TApp e1 e2 t) -> do
        e1' <- convert f e1
        e2' <- convert f e2
        t' <- f t
        return $ EApp e1' e2' t'
    (TAdd e1 e2 t) -> do
        e1' <- convert f e1
        e2' <- convert f e2
        t' <- f t
        return $ EAdd e1' e2' t'
    (TAbs i e t) -> do
        e' <- convert f e
        t' <- f t
        return $ EAbs i e' t'

run :: Infer a -> Either TypeError a
run =
    flip evalStateT mempty
        >>> flip runReaderT mempty
        >>> runExceptT
        >>> evalIntBindingT
        >>> runIdentity

infer :: RExp -> Infer (UType, TExp)
infer = \case
    (RConst (CInt i)) -> return (UTMono "Int", TConst (CInt i) (UTMono "Int"))
    (RConst (CStr str)) -> return (UTMono "String", TConst (CStr str) (UTMono "String"))
    (RAdd e1 e2) -> do
        (t1, e1') <- infer e2
        (t2, e2') <- infer e1
        t1 =:= UTMono "Int"
        t2 =:= UTMono "Int"
        return (UTMono "Int", TAdd e1' e2' (UTMono "Int"))
    (RAnn e t) -> do
        (t', e') <- infer e
        check e t'
        return (t', TAnn e' t')
    (RApp e1 e2) -> do
        (f, e1') <- infer e1
        (arg, e2') <- infer e2
        res <- fresh
        f =:= UTArrow arg res
        return (res, TApp e1' e2' res)
    (RAbs _ i e) -> do
        arg <- fresh
        withBinding i (Forall [] arg) $ do
            (res, e') <- infer e
            return $ (UTArrow arg res, TAbs i e' (UTArrow arg res))
    (RFree i) -> do
        t <- lookupSigsT i
        return (t, TFree i t)
    (RBound _ i) -> do
        t <- lookupVarT i
        return (t, TBound i t)

check :: RExp -> UType -> Infer ()
check expr t = do
    (t', _) <- infer expr
    t =:= t'
    return ()

lookupVarT :: Ident -> Infer UType
lookupVarT x@(Ident i) = do
    ctx <- ask
    maybe (throwError $ "Var - Unbound variable: " <> i) instantiate (M.lookup x ctx)

lookupSigsT :: Ident -> Infer UType
lookupSigsT x@(Ident i) = do
    ctx <- ask
    case M.lookup x ctx of
        Nothing -> trace (show ctx) (throwError $ "Sigs - Unbound variable: " <> i)
        Just ut -> return $ fromPolytype ut

insertSigs :: MonadState (Map Ident UPolytype) m => Ident -> UPolytype -> m ()
insertSigs x ty = modify (M.insert x ty)

fromPolytype :: UPolytype -> UType
fromPolytype (Forall ids ut) = ut

ucata :: Functor t => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar v) = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

withBinding :: MonadReader Ctx m => Ident -> UPolytype -> m a -> m a
withBinding x ty = local (M.insert x ty)

deriving instance Ord IntVar

class FreeVars a where
    freeVars :: a -> Infer (Set (Either Ident IntVar))

instance FreeVars UType where
    freeVars ut = do
        fuvs <- fmap (S.fromList . map Right) . lift . lift . lift $ getFreeVars ut
        let ftvs =
                ucata
                    (const S.empty)
                    (\case TMonoT x -> S.singleton (Left x); f -> fold f)
                    ut
        return $ fuvs `S.union` ftvs

instance FreeVars UPolytype where
    freeVars (Forall xs ut) = (\\ (S.fromList (map Left xs))) <$> freeVars ut

instance FreeVars Ctx where
    freeVars = fmap S.unions . mapM freeVars . M.elems

fresh :: Infer UType
fresh = UVar <$> lift (lift (lift freeVar))

instance Fallible TypeT IntVar TypeError where
    occursFailure iv ut = "Infinite"
    mismatchFailure iv ut = "Mismatch"

(=:=) :: UType -> UType -> Infer UType
(=:=) s t = lift . lift $ s U.=:= t

applyBindings :: UType -> Infer UType
applyBindings = lift . lift . U.applyBindings

instantiate :: UPolytype -> Infer UType
instantiate (Forall xs uty) = do
    xs' <- mapM (const fresh) xs
    return $ substU (M.fromList (zip (map Left xs) xs')) uty

substU :: Map (Either Ident IntVar) UType -> UType -> UType
substU m =
    ucata
        (\v -> fromMaybe (UVar v) (M.lookup (Right v) m))
        ( \case
            TPolyT v -> fromMaybe (UTPoly v) (M.lookup (Left v) m)
            f -> UTerm f
        )

skolemize :: UPolytype -> Infer UType
skolemize (Forall xs uty) = do
    xs' <- mapM (const fresh) xs
    return $ substU (M.fromList (zip (map Left xs) (map toSkolem xs'))) uty
  where
    toSkolem (UVar v) = UTPoly (mkVarName "s" v)

mkVarName :: String -> IntVar -> Ident
mkVarName nm (IntVar v) = Ident $ nm ++ show (v + (maxBound :: Int) + 1)

generalize :: UType -> Infer UPolytype
generalize uty = do
    uty' <- applyBindings uty
    ctx <- ask
    tmfvs <- freeVars uty'
    ctxfvs <- freeVars ctx
    let fvs = S.toList $ tmfvs \\ ctxfvs
        xs = map (either id (mkVarName "a")) fvs
    return $ Forall xs (substU (M.fromList (zip fvs (map UTPoly xs))) uty')

fromUPolytype :: UPolytype -> Polytype
fromUPolytype = fmap (fromJust . freeze)
