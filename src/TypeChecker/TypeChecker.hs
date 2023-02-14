{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedRecordDot #-}

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

import Data.Bool (bool)

import qualified Grammar.Abs as Old
import Grammar.ErrM (Err)

import TypeChecker.TypeCheckerIr

data Ctx = Ctx
    { env :: [Map Ident Type]
    , sigs :: Map Ident Type
    , typs :: Set Ident
    }
    deriving Show

type Check = ReaderT Ctx (ExceptT Error Identity)

initEnv :: Ctx
initEnv =
    Ctx { env = mempty
        , sigs = mempty
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

    Old.EId i -> do
        ctx <- R.ask
        case lookupEnv i ctx of
          Just t -> return t
          Nothing -> case lookupSigs i ctx of
                       Just t -> return t
                       Nothing -> throwError UnboundVar
        

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
        let int = TMono (UIdent "Int")
        case (t1, t2) of
            (TMono (UIdent "Int"), TMono (UIdent "Int")) -> return int
            (_, TMono (UIdent "Int")) -> return int
            (TMono (UIdent "Int"), _) -> return int
            (TPoly (LIdent x), TPoly (LIdent y)) -> bool (throwError TypeMismatch) (return int) (x==y)
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

-- Double check this function. It's bad and maybe wrong
subtype :: Type -> Type -> Bool
subtype (TMono t1) (TMono t2) = t1 == t2
subtype (TMono t1) (TPoly t2) = True
subtype (TPoly t2) (TMono t1) = False
subtype (TArrow t1 t2) (TArrow t3 t4) = t1 `subtype` t3 && t2 `subtype` t4
subtype _ _                   = False

lookupEnv :: Ident -> Ctx -> Maybe Type
lookupEnv i c = case env c of
    [] -> Nothing
    x : xs -> case M.lookup i x of
            Nothing -> lookupEnv i (Ctx { env = xs
                                        , sigs = c.sigs
                                        , typs = c.typs
                                        })
            Just x -> Just x

lookupSigs :: Ident -> Ctx -> Maybe Type
lookupSigs i = M.lookup i . sigs

insertEnv :: Ident -> Type -> Ctx -> Ctx
insertEnv i t c =
    case env c of
        [] -> Ctx { env = [M.insert i t mempty]
                  , sigs = c.sigs
                  , typs = c.typs
                  }

        (x : xs) -> Ctx { env = M.insert i t x : xs
                        , sigs = c.sigs
                        , typs = c.typs
                        }

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

aToInt :: Old.Exp
aToInt = Old.EAbs (Old.Ident "x") (Old.EAdd (Old.EConst (Old.CInt 3)) (Old.EConst (Old.CInt 3)))

intToInt :: Old.Exp
intToInt = Old.EAbs (Old.Ident "x") (Old.EAdd (Old.EId $ Ident "x") (Old.EConst (Old.CInt 3)))

apply :: Old.Exp
apply = Old.EApp aToInt (Old.EConst (Old.CInt 3))

{-# WARNING todo "TODO IN CODE" #-}
todo :: a
todo = error "TODO in code"
