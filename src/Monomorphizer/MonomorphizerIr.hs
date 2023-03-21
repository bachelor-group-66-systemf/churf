module Monomorphizer.MonomorphizerIr where
import           Grammar.Abs (Ident)

newtype Program = Program [Bind]
    deriving (Show, Ord, Eq)

data Bind = Bind Id [Id] ExpT | DataType Ident [Constructor]
    deriving (Show, Ord, Eq)

data Exp
    = EId  Id
    | ELit Lit
    | ELet Id ExpT ExpT
    | EApp Type ExpT ExpT
    | EAdd Type ExpT ExpT
    | ECase Type ExpT [Injection]
    deriving (Show, Ord, Eq)

data Injection = Injection Case ExpT
    deriving (Show, Ord, Eq)

data Case = CLit Lit | CatchAll
    deriving (Show, Ord, Eq)

data Constructor = Constructor Ident [Type]
    deriving (Show, Ord, Eq)

type Id  = (Ident, Type)
type ExpT = (Exp, Type)

data Lit = LInt Integer
         | LChar Char
    deriving (Show, Ord, Eq)

newtype Type = Type Ident
    deriving (Show, Ord, Eq)
