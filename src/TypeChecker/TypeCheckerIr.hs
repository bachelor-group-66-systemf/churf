{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeCheckerIr (
    module TypeChecker.TypeCheckerIr,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.String qualified
import Grammar.Print
import Prelude
import Prelude qualified as C (Eq, Ord, Read, Show)

newtype Ctx = Ctx {vars :: Map Ident Type}
    deriving (Show)

data Env = Env
    { count :: Int
    , sigs :: Map Ident (Maybe Type)
    , constructors :: Map Ident Type
    }
    deriving (Show)

type Error = String
type Subst = Map Ident Type

type Infer = StateT Env (ReaderT Ctx (ExceptT Error Identity))

newtype Program = Program [Def]
    deriving (C.Eq, C.Ord, C.Show, C.Read)

data Data = Data Ident [Constructor]
    deriving (Show, Eq, Ord, Read)

data Constructor = Constructor Ident Type
    deriving (Show, Eq, Ord, Read)

newtype TVar = MkTVar Ident
    deriving (Show, Eq, Ord, Read)

data Type
    = TLit Ident
    | TVar TVar
    | TFun Type Type
    | TAll TVar Type
    | TData Ident [Type]
    deriving (Show, Eq, Ord, Read)

data Exp
    = EId Ident
    | ELit Lit
    | ELet Bind ExpT
    | EApp ExpT ExpT
    | EAdd ExpT ExpT
    | EAbs Ident ExpT
    | ECase ExpT [Branch]
    deriving (C.Eq, C.Ord, C.Read, C.Show)

type ExpT = (Exp, Type)

data Branch = Branch (Pattern, Type) ExpT
    deriving (C.Eq, C.Ord, C.Read, C.Show)

data Pattern = PVar Id | PLit (Lit, Type) | PInj Ident [Pattern] | PCatch
    deriving (C.Eq, C.Ord, C.Show, C.Read)

data Def = DBind Bind | DData Data
    deriving (C.Eq, C.Ord, C.Read, C.Show)

type Id = (Ident, Type)

newtype Ident = Ident String
    deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Lit = LInt Integer | LChar Char
    deriving (Show, Eq, Ord, Read)

data Bind = Bind Id [Id] ExpT
    deriving (C.Eq, C.Ord, C.Show, C.Read)

instance Print Ident where
    prt _ (Ident str) = prt 0 str

instance Print [Def] where
    prt _ [] = concatD []
    prt _ (x : xs) = concatD [prt 0 x, doc (showString "\n\n"), prt 0 xs]

instance Print Data where
    prt i = \case
        Data type_ constructors -> prPrec i 0 (concatD [doc (showString "data"), prt 0 type_, doc (showString "where"), doc (showString "{"), prt 0 constructors, doc (showString "}")])

instance Print Constructor where
    prt i = \case
        Constructor uident type_ -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":"), prt 0 type_])

instance Print [Constructor] where
    prt _ [] = concatD []
    prt _ [x] = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, prt 0 xs]

instance Print Def where
    prt i (DBind bind) = prt i bind
    prt i (DData d) = prt i d

instance Print Program where
    prt i (Program sc) = prPrec i 0 $ prt 0 sc

instance Print Bind where
    prt i (Bind (name, t) args rhs) =
        prPrec i 0 $
            concatD
                [ prt 0 name
                , doc $ showString ":"
                , prt 0 t
                , doc $ showString "\n"
                , prt 0 name
                , prtIdPs 0 args
                , doc $ showString "="
                , prt 0 rhs
                ]

instance Print [Bind] where
    prt _ [] = concatD []
    prt _ [x] = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), doc (showString "\n"), prt 0 xs]

prtIdPs :: Int -> [Id] -> Doc
prtIdPs i = prPrec i 0 . concatD . map (prtIdP i)

prtId :: Int -> Id -> Doc
prtId i (name, t) =
    prPrec i 0 $
        concatD
            [ doc $ showString "("
            , prt 0 name
            , doc $ showString ":"
            , prt 0 t
            , doc $ showString ")"
            ]

prtIdP :: Int -> Id -> Doc
prtIdP i (name, t) =
    prPrec i 0 $
        concatD
            [ doc $ showString "("
            , prt 0 name
            , doc $ showString ":"
            , prt 0 t
            , doc $ showString ")"
            ]

instance Print Exp where
    prt i = \case
        EId n -> prPrec i 3 $ concatD [prt 0 n]
        ELit lit -> prPrec i 3 $ concatD [prt 0 lit]
        ELet bs e ->
            prPrec i 3 $
                concatD
                    [ doc $ showString "let"
                    , prt 0 bs
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
                    [ doc $ showString "@"
                    , prt 1 e1
                    , doc $ showString "+"
                    , prt 2 e2
                    ]
        EAbs n e ->
            prPrec i 0 $
                concatD
                    [ doc $ showString "λ"
                    , prt 0 n
                    , doc $ showString "."
                    , prt 0 e
                    ]
        ECase exp injs ->
            prPrec
                i
                0
                ( concatD
                    [ doc (showString "case")
                    , prt 0 exp
                    , doc (showString "of")
                    , doc (showString "{")
                    , prt 0 injs
                    , doc (showString "}")
                    , doc (showString ":")
                    ]
                )

instance Print ExpT where
    prt i (e, t) = concatD [doc $ showString "(", prt i e, doc (showString ":"), prt i t, doc $ showString ")"]

instance Print Branch where
    prt i = \case
        Branch (init, t) exp -> prPrec i 0 (concatD [prt 0 init, doc (showString ":"), prt 0 t, doc (showString "=>"), prt 0 exp])

instance Print Pattern where
    prt i = \case
        PVar lident -> prPrec i 0 (concatD [prtId 0 lident])
        PLit (lit, typ) -> prPrec i 0 (concatD [doc $ showString "(", prt 0 lit, doc $ showString ",", prt 0 typ, doc $ showString ")"])
        PInj uident patterns -> prPrec i 0 (concatD [prt 0 uident, prt 0 patterns])
        PCatch -> prPrec i 0 (concatD [doc (showString "_")])

instance Print [Branch] where
    prt _ [] = concatD []
    prt _ [x] = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print TVar where
    prt i (MkTVar id) = prt i id

instance Print Type where
    prt i = \case
        TLit uident -> prPrec i 2 (concatD [prt 0 uident])
        TVar tvar -> prPrec i 2 (concatD [prt 0 tvar])
        TAll tvar type_ -> prPrec i 1 (concatD [doc (showString "forall"), prt 0 tvar, doc (showString "."), prt 0 type_])
        TData ident types -> prPrec i 1 (concatD [prt 0 ident, prt 0 types])
        TFun type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])

instance Print Lit where
    prt i = \case
        LInt n -> prPrec i 0 (concatD [prt 0 n])
        LChar c -> prPrec i 0 (concatD [prt 0 c])
