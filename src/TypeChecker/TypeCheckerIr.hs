{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeChecker.TypeCheckerIr (
    module Grammar.Abs,
    module TypeChecker.TypeCheckerIr,
) where

import           Data.String   (IsString)
import           Grammar.Abs   (Lit (..))
import           Grammar.Print
import           Prelude
import qualified Prelude       as C (Eq, Ord, Read, Show)

newtype Program' t = Program [Def' t]
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

data Def' t
    = DBind (Bind' t)
    | DData (Data' t)
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

data Type
    = TLit Ident
    | TVar TVar
    | TData Ident [Type]
    | TFun Type Type
    deriving (Eq, Ord, Show, Read)

data Data' t = Data t [Inj' t]
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

data Inj' t = Inj Ident t
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

newtype Ident = Ident String
    deriving (C.Eq, C.Ord, C.Show, C.Read, IsString)

data Pattern' t
    = PVar (Id' t) -- TODO should be Ident
    | PLit (Lit, t) -- TODO should be Lit
    | PCatch
    | PEnum Ident
    | PInj Ident [Pattern' t] -- TODO should be (Pattern' t, t)
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

data Exp' t
    = EVar Ident
    | EInj Ident
    | ELit Lit
    | ELet (Bind' t) (ExpT' t)
    | EApp (ExpT' t) (ExpT' t)
    | EAdd (ExpT' t) (ExpT' t)
    | EAbs Ident (ExpT' t)
    | ECase (ExpT' t) [Branch' t]
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

newtype TVar = MkTVar Ident
    deriving (C.Eq, C.Ord, C.Show, C.Read)

type Id' t = (Ident, t)
type ExpT' t = (Exp' t, t)

data Bind' t = Bind (Id' t) [Id' t] (ExpT' t)
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

data Branch' t = Branch (Pattern' t, t) (ExpT' t)
    deriving (C.Eq, C.Ord, C.Show, C.Read, Functor)

instance Print Ident where
    prt _ (Ident s) = doc $ showString s

instance Print t => Print (Program' t) where
    prt i (Program sc) = prt i sc

instance Print t => Print (Bind' t) where
    prt i (Bind sig@(name, _) parms rhs) = concatD
                [ prtSig sig
                , prt i name
                , prt i parms
                , doc $ showString "="
                , prt i rhs
                ]

prtSig :: Print t => Id' t -> Doc
prtSig (name, t) =
    concatD
        [ prt 0 name
        , doc $ showString ":"
        , prt 0 t
        , doc $ showString ";"
        ]

instance Print t => Print (ExpT' t) where
    prt i (e, t) =
        concatD
            [ doc $ showString "("
            , prt i e
            , doc $ showString ":"
            , prt 0 t
            , doc $ showString ")"
            ]

instance Print t => Print [Bind' t] where
    prt _ []       = concatD []
    prt i [x]      = concatD [prt i x]
    prt i (x : xs) = concatD [prt i x, doc (showString ";"), prt i xs]

instance Print t => Print (Id' t) where
    prt i (name, t) =
        concatD
            [ doc $ showString "("
            , prt i name
            , doc $ showString ","
            , prt i t
            , doc $ showString ")"
            ]

instance Print t => Print (Exp' t) where
    prt i = \case
        EVar lident -> prPrec i 3 (concatD [prt 0 lident])
        EInj uident -> prPrec i 3 (concatD [prt 0 uident])
        ELit lit -> prPrec i 3 (concatD [prt 0 lit])
        EApp exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, prt 3 exp2])
        EAdd exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "+"), prt 2 exp2])
        ELet bind exp -> prPrec i 0 (concatD [doc (showString "let"), prt 0 bind, doc (showString "in"), prt 0 exp])
        EAbs lident exp -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 lident, doc (showString "."), prt 0 exp])
        ECase exp branchs -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString "of"), doc (showString "{"), prt 0 branchs, doc (showString "}")])

instance Print t => Print (Branch' t) where
    prt i (Branch (pattern_, t) exp) = prPrec i 0 (concatD [doc (showString "("), prt 0 pattern_, doc (showString " : "), prt 0 t, doc (showString ")"), doc (showString "=>"), prt 0 exp])

instance Print t => Print [Branch' t] where
    prt _ []       = concatD []
    prt _ [x]      = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print t => Print (Def' t) where
    prt i = \case
        DBind bind  -> prPrec i 0 (concatD [prt 0 bind])
        DData data_ -> prPrec i 0 (concatD [prt 0 data_])

instance Print t => Print (Data' t) where
    prt i = \case
        Data type_ injs -> prPrec i 0 (concatD [doc (showString "data"), prt 0 type_, doc (showString "where"), doc (showString "{"), prt 0 injs, doc (showString "}")])

instance Print t => Print (Inj' t) where
    prt i = \case
        Inj uident type_ -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":"), prt 0 type_])

instance Print t => Print [Inj' t] where
    prt _ [] = concatD []
    prt i [x] = prt i x
    prt i (x : xs) = prPrec i 0 $ concatD [prt i x, doc $ showString "\n  ", prt i xs]

instance Print t => Print (Pattern' t) where
    prt i = \case
        PVar name -> prPrec i 1 (concatD [prt 0 name])
        PLit (lit, _) -> prPrec i 1 (concatD [prt 0 lit])
        PCatch -> prPrec i 1 (concatD [doc (showString "_")])
        PEnum name -> prPrec i 1 (concatD [prt 0 name])
        PInj uident patterns -> prPrec i 0 (concatD [prt 0 uident, prt 1 patterns])

instance Print t => Print [Def' t] where
    prt _ []       = concatD []
    prt _ [x]      = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [Type] where
    prt _ []       = concatD []
    prt _ (x : xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print Type where
    prt i = \case
        TLit uident -> prPrec i 1 (concatD [prt 0 uident])
        TVar tvar -> prPrec i 1 (concatD [prt 0 tvar])
        TData uident types -> prPrec i 1 (concatD [prt 0 uident, doc (showString "("), prt 0 types, doc (showString ")")])
        TFun type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])

instance Print TVar where
    prt i (MkTVar ident) = prt i ident

type Program = Program' Type
type Def = Def' Type
type Data = Data' Type
type Bind = Bind' Type
type Branch = Branch' Type
type Pattern = Pattern' Type
type Inj = Inj' Type
type Exp = Exp' Type
type ExpT = ExpT' Type
type Id = Id' Type
pattern DBind' id vars expt = DBind (Bind id vars expt)
pattern DData' typ injs = DData (Data typ injs)
