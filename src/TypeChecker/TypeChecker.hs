{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedRecordDot #-}

module TypeChecker.TypeChecker (typecheck) where

import Control.Monad (when, void)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as W
import Control.Monad.State (StateT)
import qualified Control.Monad.State as St
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bool (bool)
import qualified Grammar.Abs as Old
import Grammar.ErrM (Err)

import TypeChecker.TypeCheckerIr

data Ctx = Ctx { env :: Map Ident Type
               , sigs :: Map Ident Type
               }
    deriving Show

{-

The type checker will assume we first rename all variables to unique name, as to not
have to care about scoping. It significantly improves the quality of life of the
programmer.

-}

type Check = StateT (Map Ident Type) (ReaderT Ctx (ExceptT Error Identity))

initEnv :: Ctx
initEnv =
    Ctx { env = mempty
        , sigs = mempty
        }

run :: Check Type -> Either Error Type
run = runIdentity . runExceptT . flip R.runReaderT initEnv . flip St.evalStateT mempty

typecheck :: Old.Program -> Either Error ()
typecheck = todo

inferPrg :: Old.Program -> Check ()
inferPrg (Program [x]) = void $ inferBind x

inferBind :: Old.Bind -> Check ()
inferBind (Bind _ _ e) = void $ inferExp e

inferExp :: Old.Exp -> Check Type
inferExp = \case

    -- TODO: Fix bound variable lookup
    Old.EId i -> do
        st <- St.get 
        case lookupBound i st of
          Just t -> return t
          Nothing -> do
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
        let int = TMono "Int"
        updateBound e1 int
        updateBound e2 int
        inf1 <- inferExp e1
        inf2 <- inferExp e2
        when (not $ isInt inf1 && isInt inf2) (throwError TypeMismatch)
        return int

    -- Incomplete and probably wrong
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
        St.modify (M.insert i assume)
        infT <- R.local (insertEnv i assume) (inferExp e)
        St.gets (M.lookup i) >>= \case
            Nothing -> todo
            Just x -> return (TArrow x infT)

    Old.ELet i e1 e2 -> todo

-- Aux

-- Double check this function. It's bad and maybe wrong
subtype :: Type -> Type -> Bool
subtype (TMono t1) (TMono t2) = t1 == t2
subtype (TMono t1) (TPoly t2) = True
subtype (TPoly t2) (TMono t1) = False
subtype (TArrow t1 t2) (TArrow t3 t4) = t1 `subtype` t3 && t2 `subtype` t4
subtype _ _                   = False

lookupEnv :: Ident -> Ctx -> Maybe Type
lookupEnv i = M.lookup i . env

lookupSigs :: Ident -> Ctx -> Maybe Type
lookupSigs i = M.lookup i . sigs

insertEnv :: Ident -> Type -> Ctx -> Ctx
insertEnv i t c = Ctx { env = M.insert i t c.env
                      , sigs = c.sigs
                      }

updateBound :: Old.Exp -> Type -> Check ()
updateBound (Old.EId i) t = St.modify (M.insert i t)
updateBound _ _           = return ()

isBound :: Old.Exp -> Check Bool
isBound (Old.EId i) = (M.member i) <$> St.get
isBound _           = return False

lookupBound :: Ident -> Map Ident Type -> Maybe Type
lookupBound = M.lookup

isInt :: Type -> Bool
isInt (TMono "Int") = True
isInt (TPoly _)     = True
isInt _             = False

data Error
    = TypeMismatch
    | NotNumber
    | FunctionTypeMismatch
    | NotFunction
    | UnboundVar
    | AnnotatedMismatch
    | Default
    deriving Show

-- Tests

number :: Old.Exp
number = Old.EConst (CInt 3)

aToInt :: Old.Exp
aToInt = Old.EAbs (Old.Ident "x") (Old.EAdd (Old.EConst (Old.CInt 3)) (Old.EConst (Old.CInt 3)))

intToInt :: Old.Exp
intToInt = Old.EAbs (Old.Ident "x") (Old.EAdd (Old.EId $ Ident "x") (Old.EConst (Old.CInt 3)))

addLambda :: Old.Exp
addLambda = Old.EAbs (Old.Ident "x") (Old.EAdd (Old.EId $ Ident "x") (Old.EId $ Ident "x"))

{-# WARNING todo "TODO IN CODE" #-}
todo :: a
todo = error "TODO in code"
