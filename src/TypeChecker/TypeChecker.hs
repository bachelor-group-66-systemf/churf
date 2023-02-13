{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeChecker where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as W

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import qualified Grammar.Abs as Old
import Grammar.ErrM (Err)

import TypeChecker.TypeCheckerIr

data Ctx = Ctx
    { env :: [Map Ident Type]
    , sig :: Map Ident Bind
    , typs :: Set Ident
    }

type Check a = WriterT String (ReaderT Ctx Err) a

inferExp :: Old.Exp -> Check Type
inferExp = \case
    Old.EAnn e t -> do
        infT <- inferExp e
        when (t /= infT) (throwError $ show (AnnotatedMismatch (show e) (show t) (show infT)))
        return infT
    Old.EConst c -> case c of
        (CInt i) -> return (TMono $ Old.Ident "Int")
        (CStr s) -> return (TMono $ Old.Ident "String")
    Old.EId i -> lookupEnv i
    Old.EAdd e1 e2 -> do
        t1 <- inferExp e1
        t2 <- inferExp e2
        case (t1, t2) of
            (TMono (Old.Ident "Int"), TMono (Old.Ident "Int")) -> return t1
            _ -> throwError $ show (NotNumber (show t1))
        return t1

    -- This is wrong currently. (a -> b) should be able to take String
    Old.EApp e1 e2 -> do
        inferExp e1 >>= \case
            TFun mono@(TMono i) t2 -> do
                t <- inferExp e2
                when (t /= mono) (throwError $ show $ TypeMismatch (show t) (show mono))
                return t

            -- Not entirely correct. Should sometimes be able to provide mono types where poly expected.
            -- i.e id : a -> a; id "string"
            TFun poly@(TPoly f) t2 -> do
                t <- inferExp e2
                when (t /= poly) (throwError $ show (TypeMismatch (show t) (show poly)))
                return t
            t -> throwError $ show (NotFunction "Expected a function, but got:" (show t))

    Old.EAbs i e -> undefined

    Old.ELet b e -> undefined

-- Aux

lookupEnv :: Ident -> Check Type
lookupEnv i =
    R.asks env >>= \case
        [] -> throwError $ show (UnboundVar "Variable not found" (show i))
        xs -> lookupEnv' i xs
  where
    lookupEnv' :: Ident -> [Map Ident Type] -> Check Type
    lookupEnv' i [] = throwError $ show (UnboundVar "Variable not found" (show i))
    lookupEnv' i (x : xs) = case M.lookup i x of
        Just t -> return t
        Nothing -> lookupEnv' i xs

lookupSig :: Ident -> Check Bind
lookupSig b =
    R.asks sig >>= \m -> case M.lookup b m of
        Nothing -> undefined
        Just b -> return b

insertEnv :: Ident -> Type -> Ctx -> Ctx
insertEnv i t c =
    case env c of
        [] -> Ctx{env = [M.insert i t mempty]}
        (x : xs) -> Ctx{env = M.insert i t x : xs}

data Error
    = TypeMismatch String String
    | NotNumber String
    | FunctionTypeMismatch String String String
    | NotFunction String String
    | UnboundVar String String
    | AnnotatedMismatch String String String
    | Default String

showErr :: Error -> String
showErr = \case
    TypeMismatch expected found -> unwords ["Expected type:", show expected, "but got", show found]
    NotNumber mess -> "Expected a number, but got: " <> mess
    NotFunction mess func -> mess <> ": " <> func
    FunctionTypeMismatch func expected found -> unwords ["Function:", show func, "expected:", show expected, "but got:", show found]
    UnboundVar mess var -> mess <> ": " <> var
    AnnotatedMismatch expression expected found ->
        unwords
            [ "Expression"
            , expression
            , "expected type"
            , expected
            , "but was inferred as type"
            , found
            ]
    Default mess -> mess

instance Show Error where
    show = showErr
