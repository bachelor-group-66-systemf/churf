{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TypeChecker.TypeChecker (typecheck, partitionType) where

import           Auxiliary                 (maybeToRightM, snoc)
import           Control.Monad.Except      (throwError, unless)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Grammar.Abs
import           Grammar.ErrM              (Err)
import           Grammar.Print             (Print (prt), concatD, doc,
                                            printTree, render)
import           Prelude                   hiding (exp, id)
import qualified TypeChecker.TypeCheckerIr as T

-- NOTE: this type checker is poorly tested

-- TODO
-- Coercion
-- Type inference

data Cxt = Cxt
    { env :: Map Ident Type -- ^ Local scope signature
    , sig :: Map Ident Type -- ^ Top-level signatures
    }

initCxt :: [Bind] -> Cxt
initCxt sc = Cxt { env = mempty
                 , sig = Map.fromList $ map (\(Bind n t _ _ _) -> (n, t)) sc
                 }

typecheck :: Program -> Err T.Program
typecheck (Program sc) = T.Program <$> mapM (checkBind $ initCxt sc) sc

-- | Check if infered rhs type matches type signature.
checkBind :: Cxt -> Bind -> Err T.Bind
checkBind cxt b =
    case expandLambdas b of
        Bind name t _ parms rhs -> do
            (rhs', t_rhs) <- infer cxt rhs
            unless (typeEq t_rhs t) . throwError $ typeErr name t t_rhs
            pure $ T.Bind (name, t) (zip parms ts_parms) rhs'
          where
            ts_parms = fst $ partitionType (length parms) t

-- | @ f x y = rhs ⇒ f = \x.\y. rhs @
expandLambdas :: Bind -> Bind
expandLambdas (Bind name t _ parms rhs) = Bind name t name [] rhs'
  where
    rhs'     = foldr ($) rhs $ zipWith EAbs parms ts_parms
    ts_parms = fst $ partitionType (length parms) t

-- | Infer type of expression.
infer :: Cxt -> Exp -> Err (T.Exp, Type)
infer cxt = \case
    EId x ->
        case lookupEnv x cxt of
            Nothing ->
                case lookupSig x cxt of
                    Nothing -> throwError ("Unbound variable:" ++ printTree x)
                    Just t  -> pure (T.EId (x, t), t)
            Just t -> pure (T.EId (x, t), t)

    EInt i -> pure (T.EInt i, T.TInt)

    EApp e e1 -> do
        (e', t) <- infer cxt e
        case t of
            TFun t1 t2 -> do
                e1' <- check cxt e1 t1
                pure (T.EApp t2 e' e1', t2)
            _ -> do
                throwError ("Not a function: " ++ show e)

    EAdd e e1 -> do
        e'  <- check cxt e T.TInt
        e1' <- check cxt e1 T.TInt
        pure (T.EAdd T.TInt e' e1', T.TInt)

    ESub e e1 -> do
        e'  <- check cxt e T.TInt
        e1' <- check cxt e1 T.TInt
        pure (T.ESub T.TInt e' e1', T.TInt)

    EAbs x t e  -> do
        (e', t1) <- infer (insertEnv x t cxt) e
        let t_abs = TFun t t1
        pure (T.EAbs t_abs (x, t) e', t_abs)

    ELet b e -> do
        let cxt' = insertBind b cxt
        b'    <- checkBind cxt' b
        (e', t) <- infer cxt' e
        pure (T.ELet b' e', t)

    EAnn e t -> do
        (e', t1) <- infer cxt e
        unless (typeEq t t1) $
            throwError "Inferred type and type annotation doesn't match"
        pure (e', t1)

    ECase e cs t -> do
        (e',t1) <- infer cxt e
        unless (typeEq t t1) $
            throwError "Inferred type and type annotation doesn't match"
        case traverse (\(CaseMatch c e) -> do
            -- //TODO check c as well
            e' <- check cxt e t
            unless (typeEq t t1) $
                throwError "Inferred type and type annotation doesn't match"
            pure (t1, T.Case c e')
            ) cs of
                Right cs -> pure (T.ECase t1 e' cs,t1)
                Left e   -> throwError e

-- | Check infered type matches the supplied type.
check :: Cxt -> Exp -> Type -> Err T.Exp
check cxt exp typ = case exp of
    EId x     -> do
        t <- case lookupEnv x cxt of
            Nothing -> maybeToRightM
                           ("Unbound variable:" ++ printTree x)
                           (lookupSig x cxt)
            Just t -> pure t
        unless (typeEq t typ) . throwError $ typeErr x typ t
        pure $ T.EId (x, t)

    EInt i    -> do
        unless (typeEq typ TInt) $ throwError $ typeErr i TInt typ
        pure $ T.EInt i

    EApp e e1 -> do
        (e', t) <- infer cxt e
        case t of
            TFun t1 t2 -> do
              e1' <- check cxt e1 t1
              pure $ T.EApp t2 e' e1'
            _    -> throwError ("Not a function 2: " ++ printTree e)

    EAdd e e1 -> do
        e'  <- check cxt e T.TInt
        e1' <- check cxt e1 T.TInt
        pure $ T.EAdd T.TInt e' e1'

    ESub e e1 -> do
        e'  <- check cxt e T.TInt
        e1' <- check cxt e1 T.TInt
        pure $ T.ESub T.TInt e' e1'

    EAbs x t e  -> do
        (e', t_e) <- infer (insertEnv x t cxt) e
        let t1 = TFun t t_e
        unless (typeEq t1 typ) $ throwError "Wrong lamda type!"
        pure $ T.EAbs t1 (x, t) e'

    ECase e cs t -> do
        (e',t1) <- infer cxt e
        unless (typeEq t t1) $
            throwError "Inferred type and type annotation doesn't match"
        case traverse (\(CaseMatch c e) -> do
            -- //TODO check c as well
            e' <- check cxt e t
            unless (typeEq t t1) $
                throwError "Inferred type and type annotation doesn't match"
            pure (t1, T.Case c e')
            ) cs of
                Right cs -> pure $ T.ECase t1 e' cs
                Left e   -> throwError e

    ELet b e -> do
        let cxt' = insertBind b cxt
        b' <- checkBind cxt' b
        e' <- check cxt' e typ
        pure $ T.ELet b' e'

    EAnn e t -> do
        unless (typeEq t typ) $
            throwError "Inferred type and type annotation doesn't match"
        check cxt e t

-- | Check if types are equivalent. Doesn't handle coercion or polymorphism.
typeEq :: Type -> Type -> Bool
typeEq (TFun t t1) (TFun q q1) = typeEq t q && typeEq t1 q1
typeEq t           t1          = t == t1

-- | Partion type into types of parameters and return type.
partitionType :: Int -- Number of parameters to apply
              -> Type
              -> ([Type], Type)
partitionType = go []
  where
    go acc 0 t = (acc, t)
    go acc i t = case t of
        TFun t1 t2 -> go (snoc t1 acc) (i - 1) t2
        _          -> error "Number of parameters and type doesn't match"

insertBind :: Bind -> Cxt -> Cxt
insertBind (Bind n t _ _ _) = insertEnv n t

lookupEnv :: Ident -> Cxt -> Maybe Type
lookupEnv x = Map.lookup x . env

insertEnv :: Ident -> Type -> Cxt -> Cxt
insertEnv x t cxt = cxt { env = Map.insert x t cxt.env }

lookupSig :: Ident -> Cxt -> Maybe Type
lookupSig x = Map.lookup x . sig

typeErr :: Print a => a -> Type -> Type -> String
typeErr p expected actual =  render $ concatD
    [ doc $ showString "Wrong type:", prt 0 p       , doc $ showString "\n"
    , doc $ showString "Expected:"  , prt 0 expected, doc $ showString "\n"
    , doc $ showString "Actual: "   , prt 0 actual
    ]
