{-# LANGUAGE LambdaCase #-}

module Monomorphizer.MorbIr (
    module Monomorphizer.MorbIr,
    module LambdaLifterIr
) where

import           Data.List      (intercalate)
import           Grammar.Print
import           LambdaLifterIr (Ident (..), Lit (..))
import           Prelude        hiding (exp)

newtype Program = Program [Def]
    deriving (Show, Ord, Eq)

data Def = DBind Bind | DData Data
    deriving (Show, Ord, Eq)

data Data = Data Type [Inj]
    deriving (Show, Ord, Eq)

data Bind = Bind (T Ident) [T Ident] (T Exp)
          | BindC [T Ident] (T Ident) [T Ident] (T Exp)
    deriving (Show, Ord, Eq)


type T a = (a, Type)

data Exp
    = EVar Ident
    | EVarC [T Ident] Ident
    | ELit Lit
    | ELet Bind (T Exp)
    | EApp (T Exp) (T Exp)
    | EAdd (T Exp) (T Exp)
    | ECase (T Exp) [Branch]
    deriving (Show, Ord, Eq)

data Pattern
    = PVar Ident
    | PLit Lit
    | PInj Ident [T Pattern]
    | PCatch
    | PEnum Ident
    deriving (Eq, Ord, Show)


data Branch = Branch (T Pattern) (T Exp)
    deriving (Eq, Ord, Show)

data Inj = Inj Ident Type
    deriving (Show, Ord, Eq)

data Type = TLit Ident | TFun Type Type | TData Ident [Type]

    deriving (Show, Ord, Eq)

flattenType :: Type -> [Type]
flattenType (TFun t1 t2) = t1 : flattenType t2
flattenType x            = [x]

instance Print Program where
    prt i (Program sc) = prPrec i 0 $ prt 0 sc

instance Print Bind where
    prt i (Bind sig@(name, _) parms rhs) =
        prPrec i 0 $
            concatD
                [ prt 0 sig
                , prt 0 name
                , prt 0 parms
                , doc $ showString "="
                , prt 0 rhs
                ]

    prt i (BindC cxt sig parms rhs) =
        prPrec i 0 $
            concatD
                [ doc . showString $ "{" ++ intercalate ", " (map (\(x, _) -> printTree x ++ "^") cxt) ++ "}" ++ printTree sig
                , prt i parms
                , doc $ showString "="
                , prt i rhs
                ]

instance Print [Bind] where
    prt _ []       = concatD []
    prt _ [x]      = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print Exp where
    prt i = \case
        EVar name -> prPrec i 3 $ prt 0 name
        EVarC as lident -> doc . showString
                               $ "{" ++ intercalate ", " (map go as) ++ "}" ++ printTree lident
                where
              go (x, _) = printTree x ++ "^=" ++ printTree (EVar x)
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
    prt i (Branch patt exp) = prPrec i 0 (concatD [prt i patt, doc (showString "=>"), prt 0 exp])

instance Print [Branch] where
    prt _ []       = concatD []
    prt _ [x]      = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print Def where
    prt i = \case
        DBind bind  -> prPrec i 0 (concatD [prt 0 bind])
        DData data_ -> prPrec i 0 (concatD [prt 0 data_])

instance Print Data where
    prt i = \case
        Data type_ injs -> prPrec i 0 (concatD [doc (showString "data"), prt 0 type_, doc (showString "where"), doc (showString "{"), prt 0 injs, doc (showString "}")])

instance Print Inj where
    prt i = \case
        Inj uident type_ -> prt i (uident, type_)

instance Print Pattern where
    prt i = \case
        PVar name -> prPrec i 1 (concatD [prt 0 name])
        PLit lit -> prPrec i 1 (concatD [prt 0 lit])
        PCatch -> prPrec i 1 (concatD [doc (showString "_")])
        PEnum name -> prPrec i 1 (concatD [prt 0 name])
        PInj uident patterns -> prPrec i 0 (concatD [prt 0 uident, prt 1 patterns])

instance Print [Def] where
    prt _ []       = concatD []
    prt _ [x]      = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [Type] where
    prt _ []       = concatD []
    prt _ (x : xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print Type where
    prt i = \case
        TLit uident -> prPrec i 1 (concatD [prt 0 uident])
        TFun type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])
        TData uident types -> prPrec i 1 (concatD [prt 0 uident, doc (showString "("), prt 0 types, doc (showString ")")])
