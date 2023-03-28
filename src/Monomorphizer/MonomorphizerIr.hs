{-# LANGUAGE LambdaCase #-}

module Monomorphizer.MonomorphizerIr (module Monomorphizer.MonomorphizerIr) where

import Grammar.Print
import TypeChecker.TypeCheckerIr qualified as TIR (Ident (..))

type Id = (TIR.Ident, Type)

newtype Program = Program [Def]
    deriving (Show, Ord, Eq)

data Def = DBind Bind | DData Data
    deriving (Show, Ord, Eq)

data Data = Data Type [Inj]
    deriving (Show, Ord, Eq)

data Bind = Bind Id [Id] ExpT
    deriving (Show, Ord, Eq)

data Exp
    = EVar TIR.Ident
    | ELit Lit
    | ELet Bind ExpT
    | EApp ExpT ExpT
    | EAdd ExpT ExpT
    | ECase ExpT [Branch]
    deriving (Show, Ord, Eq)

data Pattern
    = PVar Id
    | PLit (Lit, Type)
    | PInj TIR.Ident [Pattern]
    | PCatch
    | PEnum TIR.Ident
    deriving (Eq, Ord, Show)

data Branch = Branch (Pattern, Type) ExpT
    deriving (Eq, Ord, Show)

type ExpT = (Exp, Type)

data Inj = Inj TIR.Ident Type
    deriving (Show, Ord, Eq)

data Lit
    = LInt Integer
    | LChar Char
    deriving (Show, Ord, Eq)

data Type = TLit TIR.Ident | TFun Type Type
    deriving (Show, Ord, Eq)

flattenType :: Type -> [Type]
flattenType (TFun t1 t2) = t1 : flattenType t2
flattenType x = [x]

instance Print Program where
    prt i (Program sc) = prPrec i 0 $ prt 0 sc

instance Print (Bind) where
    prt i (Bind sig@(name, _) parms rhs) =
        prPrec i 0 $
            concatD
                [ prtSig sig
                , prt 0 name
                , prtIdPs 0 parms
                , doc $ showString "="
                , prt 0 rhs
                ]

prtSig :: Id -> Doc
prtSig (name, t) =
    concatD
        [ prt 0 name
        , doc $ showString ":"
        , prt 0 t
        , doc $ showString ";"
        ]

instance Print (ExpT) where
    prt i (e, t) =
        concatD
            [ doc $ showString "("
            , prt i e
            , doc $ showString ","
            , prt i t
            , doc $ showString ")"
            ]

instance Print [Bind] where
    prt _ [] = concatD []
    prt _ [x] = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

prtIdPs :: Int -> [Id] -> Doc
prtIdPs i = prPrec i 0 . concatD . map (prt i)

instance Print Exp where
    prt i = \case
        EVar name -> prPrec i 3 $ prt 0 name
        ELit lit -> prPrec i 3 $ prt 0 lit
        ELet b e ->
            prPrec i 3 $
                concatD
                    [ doc $ showString "let"
                    , prt 0 b
                    , doc $ showString "in"
                    , prt 0 e
                    ]
        EApp e1 e2 ->
            prPrec i 2 $
                concatD
                    [ prt 2 e1
                    , prt 3 e2
                    ]
        EAdd e1 e2 ->
            prPrec i 1 $
                concatD
                    [ prt 1 e1
                    , doc $ showString "+"
                    , prt 2 e2
                    ]
        ECase e branches ->
            prPrec i 0 $
                concatD
                    [ doc $ showString "case"
                    , prt 0 e
                    , doc $ showString "of"
                    , doc $ showString "{"
                    , prt 0 branches
                    , doc $ showString "}"
                    ]

instance Print Branch where
    prt i (Branch (pattern_, t) exp) = prPrec i 0 (concatD [doc (showString "("), prt 0 pattern_, doc (showString " : "), prt 0 t, doc (showString ")"), doc (showString "=>"), prt 0 exp])

instance Print [Branch] where
    prt _ [] = concatD []
    prt _ [x] = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print Def where
    prt i = \case
        DBind bind -> prPrec i 0 (concatD [prt 0 bind])
        DData data_ -> prPrec i 0 (concatD [prt 0 data_])

instance Print Data where
    prt i = \case
        Data type_ injs -> prPrec i 0 (concatD [doc (showString "data"), prt 0 type_, doc (showString "where"), doc (showString "{"), prt 0 injs, doc (showString "}")])

instance Print Inj where
    prt i = \case
        Inj uident type_ -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":"), prt 0 type_])

instance Print Pattern where
    prt i = \case
        PVar name -> prPrec i 1 (concatD [prt 0 name])
        PLit (lit, _) -> prPrec i 1 (concatD [prt 0 lit])
        PCatch -> prPrec i 1 (concatD [doc (showString "_")])
        PEnum name -> prPrec i 1 (concatD [prt 0 name])
        PInj uident patterns -> prPrec i 0 (concatD [prt 0 uident, prt 1 patterns])

instance Print [Def] where
    prt _ [] = concatD []
    prt _ [x] = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [Type] where
    prt _ [] = concatD []
    prt _ (x : xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print Type where
    prt i = \case
        TLit uident -> prPrec i 1 (concatD [prt 0 uident])
        TFun type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])

instance Print Lit where
    prt i = \case
        LInt int -> prt i int
        LChar char -> prt i char
