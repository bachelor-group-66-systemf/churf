module Monomorphizer.MonomorphizerIr (module Monomorphizer.MonomorphizerIr, module RE1, module RE2) where

import qualified Grammar.Abs               as RE1 (Data (..), Ident (..),
                                                   Init (..))
import qualified TypeChecker.TypeCheckerIr as RE2 (ExpT, Id, Indexed)

import           Grammar.Abs               (Data (..), Ident (..), Init (..))
import           TypeChecker.TypeCheckerIr (ExpT, Id, Indexed)


newtype Program = Program [Def]
    deriving (Show, Ord, Eq)

data Def = DBind Bind | DData Data
    deriving (Show, Ord, Eq)

data Bind = Bind Id [Id] ExpT
    deriving (Show, Ord, Eq)

data Exp
    = EId Id
    | ELit Lit
    | ELet Id ExpT ExpT
    | EApp Type ExpT ExpT
    | EAdd Type ExpT ExpT
    | ECase Type ExpT [Injection]
    deriving (Show, Ord, Eq)

data Injection = Injection (Init, Type) ExpT
    deriving (Eq, Ord, Show)

data Constructor = Constructor Ident [Type]
    deriving (Show, Ord, Eq)

data Lit
    = LInt Integer
    | LChar Char
    deriving (Show, Ord, Eq)

newtype Type = Type Ident
    deriving (Show, Ord, Eq)
