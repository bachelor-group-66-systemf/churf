module Monomorphizer.MonomorphizerIr (module Monomorphizer.MonomorphizerIr, module RE, module GA) where

import           Grammar.Abs               (Ident (..), Init (..), UIdent)
import qualified Grammar.Abs               as GA (Ident (..), Init (..))
import qualified TypeChecker.TypeCheckerIr as RE (Indexed)
import           TypeChecker.TypeCheckerIr (Indexed)

type Id = (Ident, Type)

newtype Program = Program [Def]
    deriving (Show, Ord, Eq)

data Def = DBind Bind | DData Constructor
    deriving (Show, Ord, Eq)

data Bind = Bind Id [Id] ExpT
    deriving (Show, Ord, Eq)

data Exp
    = EId Ident
    | ELit Lit
    | ELet Bind ExpT
    | EApp ExpT ExpT
    | EAdd ExpT ExpT
    | ECase ExpT [Injection]
    deriving (Show, Ord, Eq)

data Injection = Injection (Init, Type) ExpT
    deriving (Eq, Ord, Show)

type ExpT = (Exp, Type)

data Constructor = Constructor UIdent [(UIdent, Type)]
    deriving (Show, Ord, Eq)

data Lit
    = LInt Integer
    | LChar Char
    deriving (Show, Ord, Eq)

data Type = TLit Ident | TFun Type Type
    deriving (Show, Ord, Eq)

flattenType :: Type -> [Type]
flattenType (TFun t1 t2) = t1 : flattenType t2
flattenType x            = [x]
