module Monomorphizer.MonomorphizerIr (module Monomorphizer.MonomorphizerIr, module RE, module GA) where

import Grammar.Abs (Data, Ident, Init)
import Grammar.Abs qualified as GA (Data, Ident, Init)
import TypeChecker.TypeCheckerIr (ExpT, Id, Indexed)
import TypeChecker.TypeCheckerIr qualified as RE (ExpT, Id, Indexed)

newtype Program = Program [Def]
    deriving (Show, Ord, Eq)

data Def = DBind Bind | DData Data
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

data Constructor = Constructor Ident [Type]
    deriving (Show, Ord, Eq)

data Lit
    = LInt Integer
    | LChar Char
    deriving (Show, Ord, Eq)

data Type = TLit Ident | TFun Type Type
    deriving (Show, Ord, Eq)
