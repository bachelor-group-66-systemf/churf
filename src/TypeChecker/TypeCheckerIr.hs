{-# LANGUAGE LambdaCase #-}

module TypeChecker.TypeCheckerIr where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Grammar.Abs (
    Data (..),
    Ident (..),
    Init (..),
    Lit (..),
    TVar (..),
 )
import Grammar.Abs qualified as GA (Type (..))
import Grammar.Print
import Prelude
import Prelude qualified as C (Eq, Ord, Read, Show)

-- | A data type representing type variables
data Poly = Forall [Ident] Type
    deriving (Show)

newtype Ctx = Ctx {vars :: Map Ident Poly}
    deriving (Show)

data Env = Env
    { count :: Int
    , sigs :: Map Ident GA.Type
    , constructors :: Map Ident GA.Type
    }
    deriving (Show)

type Error = String
type Subst = Map Ident Type

type Infer = StateT Env (ReaderT Ctx (ExceptT Error Identity))

newtype Program = Program [Def]
    deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type
    = TLit Ident
    | TVar TVar
    | TFun Type Type
    | TAll TVar Type
    | TIndexed Indexed
    deriving (Show, Eq, Ord, Read)

data Exp
    = EId Id
    | ELit Lit
    | ELet Bind ExpT
    | EApp ExpT ExpT
    | EAdd ExpT ExpT
    | EAbs Id ExpT
    | ECase ExpT [Inj]
    deriving (C.Eq, C.Ord, C.Read, C.Show)

type ExpT = (Exp, Type)

data Indexed = Indexed Ident [Type]
    deriving (Show, Read, Ord, Eq)

data Inj = Inj (Init, Type) ExpT
    deriving (C.Eq, C.Ord, C.Read, C.Show)

data Def = DBind Bind | DData Data
    deriving (C.Eq, C.Ord, C.Read, C.Show)

type Id = (Ident, Type)

data Bind = Bind Id [Id] ExpT
    deriving (C.Eq, C.Ord, C.Show, C.Read)

instance Print [Def] where
    prt _ [] = concatD []
    prt _ (x : xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print Def where
    prt i (DBind bind) = prt i bind
    prt i (DData d) = prt i d

instance Print Program where
    prt i (Program sc) = prPrec i 0 $ prt 0 sc

instance Print Bind where
    prt i (Bind (t, name) args rhs) =
        prPrec i 0 $
            concatD
                [ prt 0 name
                , doc $ showString ":"
                , prt 0 t
                , doc $ showString "\n"
                , prt 0 name
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
            [ prt 0 name
            , doc $ showString ":"
            , prt 0 t
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
        EId n -> prPrec i 3 $ concatD [prtId 0 n, doc $ showString "\n"]
        ELit _ lit -> prPrec i 3 $ concatD [prt 0 lit, doc $ showString "\n"]
        ELet bs e ->
            prPrec i 3 $
                concatD
                    [ doc $ showString "let"
                    , prt 0 bs
                    , doc $ showString "in"
                    , prt 0 e
                    , doc $ showString "\n"
                    ]
        EApp _ e1 e2 ->
            prPrec i 2 $
                concatD
                    [ prt 2 e1
                    , prt 3 e2
                    ]
        EAdd t e1 e2 ->
            prPrec i 1 $
                concatD
                    [ doc $ showString "@"
                    , prt 0 t
                    , prt 1 e1
                    , doc $ showString "+"
                    , prt 2 e2
                    , doc $ showString "\n"
                    ]
        EAbs t n e ->
            prPrec i 0 $
                concatD
                    [ doc $ showString "@"
                    , prt 0 t
                    , doc $ showString "\\"
                    , prtId 0 n
                    , doc $ showString "."
                    , prt 0 e
                    , doc $ showString "\n"
                    ]
        ECase t exp injs ->
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
                    , prt 0 t
                    , doc $ showString "\n"
                    ]
                )

instance Print ExpT where
    prt i (e, t) = concatD [prt i e, doc (showString ":"), prt i t]

instance Print Inj where
    prt i = \case
        Inj (init, t) exp -> prPrec i 0 (concatD [prt 0 init, doc (showString ":"), prt 0 t, doc (showString "=>"), prt 0 exp])

instance Print [Inj] where
    prt _ [] = concatD []
    prt _ [x] = concatD [prt 0 x]
    prt _ (x : xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print Type where
    prt i = \case
        TLit uident -> prPrec i 2 (concatD [prt 0 uident])
        TVar tvar -> prPrec i 2 (concatD [prt 0 tvar])
        TAll tvar type_ -> prPrec i 1 (concatD [doc (showString "forall"), prt 0 tvar, doc (showString "."), prt 0 type_])
        TIndexed indexed -> prPrec i 1 (concatD [prt 0 indexed])
        TFun type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])

instance Print Indexed where
    prt i (Indexed u ts) = concatD [prt i u, prt i ts]
