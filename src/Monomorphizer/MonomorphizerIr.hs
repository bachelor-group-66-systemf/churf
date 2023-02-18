module Monomorphizer.MonomorphizerIr
  ( module Monomorphizer.MonomorphizerIr
  , module TypeChecker.TypeCheckerIr
  ) where

import           Grammar.Abs               (Character)
import           TypeChecker.TypeCheckerIr (Ident (..))

type Id = (Ident, Type)

newtype Program = Program [Def]
    deriving (Show, Ord, Eq)

data Def = DBind Bind | DData Data
    deriving (Show, Ord, Eq)

data Data = Data Type [Inj]
    deriving (Show, Ord, Eq)

data Bind = Bind Id [Id] ExpT
    deriving (Show, Ord, Eq)

data Exp
    = EVar Ident
    | ELit Lit
    | ELet Bind ExpT
    | EApp ExpT ExpT
    | EAdd ExpT ExpT
    | ECase ExpT [Branch]
    deriving (Show, Ord, Eq)

data Pattern = PVar Id | PLit (Lit, Type) | PInj Ident [Pattern] | PCatch | PEnum Ident
    deriving (Eq, Ord, Show)

data Branch = Branch (Pattern, Type) ExpT
    deriving (Eq, Ord, Show)

type ExpT = (Exp, Type)

data Inj = Inj Ident Type
    deriving (Show, Ord, Eq)

data Lit
    = LInt Integer
    | LChar Character
    deriving (Show, Ord, Eq)

data Type = TLit Ident | TFun Type Type
    deriving (Show, Ord, Eq)

flattenType :: Type -> [Type]
flattenType (TFun t1 t2) = t1 : flattenType t2
flattenType x            = [x]
