module Monomorphizer.MonomorphizerIr (module Monomorphizer.MonomorphizerIr, module RE, module GA) where

import           Grammar.Abs               (Ident (..), UIdent)
import qualified Grammar.Abs               as GA (Ident (..))
import qualified TypeChecker.TypeCheckerIr as RE

type Id = (Ident, Type)

newtype Program = Program [Def]
    deriving (Show, Ord, Eq)

data Def = DBind Bind | DData Data
    deriving (Show, Ord, Eq)

data Data = Data Type [Constructor]
    deriving (Show, Ord, Eq)

data Bind = Bind Id [Id] ExpT
    deriving (Show, Ord, Eq)

data Exp
    = EId Ident
    | ELit Lit
    | ELet Bind ExpT
    | EApp ExpT ExpT
    | EAdd ExpT ExpT
    | ECase ExpT [Branch]
    deriving (Show, Ord, Eq)

data Pattern = PVar Id | PLit (Lit, Type) | PInj Ident [Pattern] | PCatch
    deriving (Eq, Ord, Show)

data Branch = Branch (Pattern, Type) ExpT
    deriving (Eq, Ord, Show)

type ExpT = (Exp, Type)

data Constructor = Constructor Ident [(Ident, Type)]
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
