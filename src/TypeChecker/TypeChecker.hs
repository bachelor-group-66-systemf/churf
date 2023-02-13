{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module TypeChecker.TypeChecker (typecheck) where

import Control.Monad (when, void)
import Control.Monad.Except (ExceptT, throwError, runExceptT)

import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R

import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as W

import Data.Functor.Identity (Identity, runIdentity)

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
    deriving Show

type Check = ReaderT Ctx (ExceptT Error Identity)

initEnv :: Ctx
initEnv =
    Ctx { env = mempty
        , sig = mempty
        , typs = mempty
        }

run :: Check Type -> Either Error Type
run = runIdentity . runExceptT . flip R.runReaderT initEnv

typecheck :: Old.Program -> Either Error ()
typecheck = runIdentity . runExceptT . flip R.runReaderT initEnv . inferPrg

inferPrg :: Old.Program -> Check ()
inferPrg (Program [x]) = void $ inferBind x

inferBind :: Old.Bind -> Check ()
inferBind (Bind _ _ e) = void $ inferExp e

inferExp :: Old.Exp -> Check Type
inferExp = \case

    Old.EId i -> undefined

    Old.EAnn e t -> do
        infT <- inferExp e
        when (t /= infT) (throwError AnnotatedMismatch)
        return infT

    Old.EConst c -> case c of
        (Old.CInt i) -> return (TMono $ UIdent "Int")
        (Old.CStr s) -> return (TMono $ UIdent "String")

    Old.EAdd e1 e2 -> do
        t1 <- inferExp e1
        t2 <- inferExp e2
        case (t1, t2) of
            (TMono (UIdent "Int"), TMono (UIdent "Int")) -> return t1
            _ -> throwError NotNumber
        return t1

    Old.EApp e1 e2 -> do
        inferExp e1 >>= \case
            TArrow mono@(TMono i) t2 -> do
                t <- inferExp e2
                when (t /= mono) (throwError TypeMismatch)
                return t2

            TArrow poly@(TPoly f) t2 -> do
                t <- inferExp e2 
                when (not $ t `subtype` t) (throwError TypeMismatch)
                return t2

-- This is not entirely correct. The assumed type can change.
    Old.EAbs i e -> do
        let assume = (TPoly "a")
        infT <- R.local (insertEnv i assume) (inferExp e)
        return (TArrow assume infT)

    Old.ELet i e1 e2 -> undefined

-- Aux

subtype :: Type -> Type -> Bool
subtype (TMono t1) (TMono t2) = t1 == t2
subtype (TMono t1) (TPoly t2) = True
subtype (TPoly t2) (TMono t1) = False
subtype (TArrow t1 t2) (TArrow t3 t4) = t1 `subtype` t3 && t2 `subtype` t4

lookupEnv :: Ident -> Ctx -> Maybe Type
lookupEnv i c = case env c of
    [] -> Nothing
    x : xs -> case M.lookup i x of
            Nothing -> lookupEnv i (Ctx { env = xs })
            Just x -> Just x

lookupSig :: Ident -> Ctx -> Maybe Bind
lookupSig i = M.lookup i . sig

insertEnv :: Ident -> Type -> Ctx -> Ctx
insertEnv i t c =
    case env c of
        [] -> Ctx{env = [M.insert i t mempty]}
        (x : xs) -> Ctx{env = M.insert i t x : xs}

data Error
    = TypeMismatch
    | NotNumber
    | FunctionTypeMismatch
    | NotFunction
    | UnboundVar
    | AnnotatedMismatch
    | Default
    deriving Show

-- showErr :: Error -> String
-- showErr = \case
--     TypeMismatch expected found -> unwords ["Expected type:", show expected, "but got", show found]
--     NotNumber mess -> "Expected a number, but got: " <> mess
--     NotFunction mess func -> mess <> ": " <> func
--     FunctionTypeMismatch func expected found -> unwords ["Function:", show func, "expected:", show expected, "but got:", show found]
--     UnboundVar mess var -> mess <> ": " <> var
--     AnnotatedMismatch expression expected found ->
--         unwords
--             [ "Expression"
--             , expression
--             , "expected type"
--             , expected
--             , "but was inferred as type"
--             , found
--             ]
--     Default mess -> mess


-- Tests

number :: Old.Exp
number = Old.EConst (CInt 3)

lambda :: Old.Exp
lambda = Old.EAbs (Old.Ident "x") (Old.EAdd (Old.EConst (Old.CInt 3)) (Old.EConst (Old.CInt 3)))

apply :: Old.Exp
apply = Old.EApp lambda (Old.EConst (Old.CInt 3))

{-# WARNING todo "TODO IN CODE" #-}
todo :: a
todo = error "TODO in code"
