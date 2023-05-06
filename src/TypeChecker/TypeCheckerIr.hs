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

newtype Program' t = Program [Def' t]
    deriving (Eq, Ord, Show, Functor)

data Def' t
    = DBind (Bind' t)
    | DData (Data' t)
    deriving (Eq, Ord, Show, Functor)

data Type
    = TLit Ident
    | TVar TVar
    | TData Ident [Type]
    | TFun Type Type
    deriving (Eq, Ord, Show)

data Data' t = Data t [Inj' t]
    deriving (Eq, Ord, Show, Functor)

data Inj' t = Inj Ident t
    deriving (Eq, Ord, Show, Functor)

newtype Ident = Ident String
    deriving (Eq, Ord, Show, IsString)

data Pattern' t
    = PVar Ident
    | PLit Lit
    | PCatch
    | PEnum Ident
    | PInj Ident [(Pattern' t, t)]
    deriving (Eq, Ord, Show, Functor)

data Exp' t
    = EVar Ident
    | EInj Ident
    | ELit Lit
    | ELet (Bind' t) (T' Exp' t)
    | EApp (T' Exp' t) (T' Exp' t)
    | EAdd (T' Exp' t) (T' Exp' t)
    | EAbs Ident (T' Exp' t)
    | ECase (T' Exp' t) [Branch' t]
    deriving (Eq, Ord, Show, Functor)

newtype TVar = MkTVar Ident
    deriving (Eq, Ord, Show)

type T' a t = (a t, t)
type T  a t = (a, t)


data Bind' t = Bind (T Ident t) [T Ident t] (T' Exp' t)
    deriving (Eq, Ord, Show, Functor)

data Branch' t = Branch (T' Pattern' t) (T' Exp' t)
    deriving (Eq, Ord, Show, Functor)

instance Print Ident where
    prt _ (Ident s) = doc $ showString s

instance Print t => Print (Program' t) where
    prt i (Program sc) = prt i sc

instance Print t => Print (Bind' t) where
    prt i (Bind sig parms rhs) = concatD
                [ prtSig sig
                , prt i parms
                , doc $ showString "="
                , prt i rhs
                ]

prtSig :: Print t => T Ident t -> Doc
prtSig (x, t) =
    concatD
        [ prt 0 x
        , doc $ showString ":"
        , prt 0 t
        ]

instance (Print a, Print t) => Print (T a t) where
    prt i (x, t) =
        concatD
            [ -- doc $ showString "("
            {- , -} prt i x
--            , doc $ showString ":"
--            , prt 0 t
--            , doc $ showString ")"
            ]

instance Print t => Print [Bind' t] where
    prt _ []       = concatD []
    prt i [x]      = concatD [prt i x]
    prt i (x : xs) = concatD [prt i x, doc (showString ";"), prt i xs]

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
        PLit lit -> prPrec i 1 (concatD [prt 0 lit])
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
pattern TVar' s = TVar (MkTVar s)
pattern DBind' id vars expt = DBind (Bind id vars expt)
pattern DData' typ injs = DData (Data typ injs)
